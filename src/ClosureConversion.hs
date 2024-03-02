{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ClosureConversion (translClosure) where

import CPS
import CompileEnv
import Control.Lens.Plated (transformOn)
import Control.Monad.Morph
import Control.Monad.State
import Data.List ((\\))
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Util (free, var)
import GHC.OldList (nub)

rename :: Name -> Name -> Term -> Term
rename a b = transformOn var f
  where
    f x = if x == a then b else x

renames :: [Name] -> [Name] -> Term -> Term
renames (a : as) (b : bs) t = renames as bs (rename a b t)
renames [] [] t = t
renames _ _ _ = error ""

wrapProj :: Int -> Name -> [Name] -> Term -> Term
wrapProj base env closure = f (zip [base..] closure)
  where
    f ((i, v) : xs) acc = f xs (LetSel v i env acc)
    f [] acc = acc

type FnClosure = Map.Map Name Name

findEnv :: Name -> State FnClosure Name
findEnv n = gets (fromMaybe n . Map.lookup n)

addEnv :: Name -> Name -> State FnClosure ()
addEnv f env = modify (Map.insert f env)


transl :: Term -> CompEnvT (State FnClosure) Term
-- function
--
-- let f = fun k x -> l in m ~~>
--   let code = fun k env x ->
--                 let nfv1 = select 0 env in
--                 let nfvn = select n env in l
--    in let f = (code, nfv ...) in m
transl (LetVal n (Fn k Nothing xs l) m) = do
  let fvars = free l \\ (k : xs)
  nfvars <- mapM uniName fvars
  code <- freshStr ("code_" ++ baseStr n)
  env <- freshStr "env"
  l' <- wrapProj 1 env nfvars <$> (renames fvars nfvars <$> transl l)
  m' <- transl m
  pure $
    LetVal
      code
      (Fn k (Just env) xs l')
      (LetVal n (Tuple (code : fvars)) m')
-- For functions declared in letrec they can share same closure.
-- The env name of those function can be find in FnClosure.
transl (LetFns fns m) = do
    let fnames = map fst fns
    fcodes <- mapM uniName fnames
    let fvars = nub $ concatMap freeVarsOfFn fns \\ fnames
    nfvars <- mapM uniName fvars
    m' <- transl m
    fns' <- mapM (translFn fvars nfvars fnames fcodes) (zip fcodes fns)
    env <- freshStr "env_"
    pure $ LetFns fns' (LetVal env (Tuple fvars) (wrapFixed env fnames fcodes m'))
  where
    freeVarsOfFn (_, Fn k Nothing xs l) = free l \\ (k:xs)

    translFn fvs nfvs fnames fcodes (fcode, (_, Fn k Nothing xs l)) = do
      env <- freshStr "env"
      env' <- freshStr "env"
      l' <- renames fvs nfvs <$> transl l
      pure (fcode, Fn k (Just env) xs
             (LetSel env' 1 env  
             (wrapFixed env' fnames fcodes
              (wrapProj 0 env' nfvs l'))))

    wrapFixed _ [] [] p = p
    wrapFixed env (name:names) (code:codes) p = LetVal name (Tuple [code,env]) (wrapFixed env names codes p)

-- f k x ~~>
--    let f = select 0 env
--     in m[f k x |-> let f' = select 0 f in f' k f x]
--
transl (Apply g k Nothing xs) = do
  env <- lift $ findEnv g
  code <- fresh
  pure $ LetSel code 0 env (Apply code k (Just env) xs)

transl (Raise effect k args) =
  pure (Raise effect k args)


-- continuation
--
-- let k x = l in m ~~>
--     let code env x =
--          let fvar1 = select 0 env in
--          let fvarN = select N env in l
--     in
--     let k = (code, fvar ...)
--     in m[k x |-> let k' = select 0 k in k' k x]
-- transl (LetCont k Nothing x l m) = do
--   let fvars = free l \\ [k, x]
--   nfvars <- mapM uniName fvars
--   code <- freshStr ("code_" ++ baseStr k)
--   env <- freshStr "env"
--   l' <- wrapProj 1 env nfvars <$> (renames fvars nfvars <$> transl l)
--   m' <- transl m
--   pure $
--     LetCont code (Just env) x l' (LetVal k (Tuple (code : fvars)) m')
-- transl (Continue k Nothing x) = do
--   let env = k
--   kname <- freshStr "k"
--   pure $ LetSel kname 0 env (Continue kname (Just env) x)
transl (LetCont k x l m) = LetCont k x <$> transl l <*> transl m
transl (Continue k x) = pure $ Continue k x
transl (LetVal n1 v l) = LetVal n1 v <$> transl l
transl (LetSel n1 i n2 l) = LetSel n1 i n2 <$> transl l
transl (LetPrim n1 p ns l) = LetPrim n1 p ns <$> transl l
transl (Switch n ix ks fb) = Switch n ix <$> mapM transl ks <*> mapM transl fb
transl (Handle m hdls) = Handle <$> transl m <*> pure hdls
transl term@(Raise {}) = pure term
transl term@(Halt {}) = pure term
transl term = pure term

-- transl x = error $ "unexpected " ++ show x

translClosure :: Term -> CompEnv Term
translClosure t = hoist (`evalStateT` Map.empty) (transl t)


