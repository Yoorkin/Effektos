{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Simp (simplify) where

import CPS
import CompileEnv
import Control.Lens (transformOn)
import Control.Monad.State (runStateT)
import Control.Monad.State.Lazy (evalState, evalStateT)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowWith, traceWith)
import Primitive
import Util (def, usage, var)
import Prelude hiding (lookup)

type Census = Map.Map Name Int

type Env = Map.Map Name Value

type Subst = Map.Map Name Name

count :: (Num a, Ord k) => Map.Map k a -> k -> a
count census x = fromMaybe 1 (Map.lookup x census)

addEnv :: Env -> Name -> Value -> Env
addEnv env x v = Map.insert x v env

lookup :: (Ord k) => Map.Map k a -> k -> Maybe a
lookup env x = Map.lookup x env

applySubst :: (Ord k) => Map.Map k k -> k -> k
applySubst s x = fromMaybe x (Map.lookup x s)

extendSubst :: (Ord k) => Map.Map k a -> k -> a -> Map.Map k a
extendSubst st s t = Map.insert s t st

extendSubsts :: Subst -> [(Name, Name)] -> Subst
extendSubsts st [] = st
extendSubsts st ((s, t) : xs) = extendSubsts (extendSubst st s t) xs

simpVal :: Census -> Env -> Subst -> Value -> CompEnv Value
simpVal census env s v = case v of
  Var n -> pure $ Var (applySubst s n)
  Tuple ns -> pure $ Tuple (map (applySubst s) ns)
  Cont n l -> Cont n <$> simp census env s l
  Fn k _ x l -> Fn k Nothing x <$> simp census env s l
  _ -> pure v

inst :: Term -> CompEnv Term
inst p =
  do
    let defs = def p
    ndefs <- mapM uniName defs
    let f x = fromMaybe x (Map.lookup x (Map.fromList $ zip defs ndefs))
    let r = transformOn var f p
    pure r

traceSimp :: Term -> CompEnv Term -> CompEnv Term
traceSimp p =
  traceWith
    ( \x ->
        let r = evalState x (mkCompBeginWith 1000)
         in "---------- simp --------\n"
              ++ "===> before\n"
              ++ show p
              ++ "\n===> after\n"
              ++ show r
              ++ "\n"
    )

simp :: Census -> Env -> Subst -> Term -> CompEnv Term
simp census env s p =
  case p of
    LetVal x v l ->
      if count census x == 0
        then simp census env s l
        else do
          v' <- simpVal census env s v
          LetVal x v' <$> simp census (addEnv env x v') s l
    LetSel x i y l ->
      let y' = applySubst s y
       in case lookup env y' of
            Just (Tuple elems) -> simp census env (extendSubst s x (elems !! i)) l
            _ -> LetSel x i y' <$> simp census env s l
    LetCont k x l m -> do
      l' <- simp census env s l
      case count census k of
        0 -> simp census env s m
        -- 1 -> simp census (addEnv env k (Cont x l')) s m
        _ -> LetCont k x l' <$> simp census (addEnv env k (Cont x l')) s m
    LetFns fns m -> do
      fns' <- mapM (\(x, b) -> (x,) <$> simpVal census env s b) fns
      m' <- simp census env s m
      pure $ LetFns fns' m'
    Continue k x ->
      -- traceSimp p $
      let x' = applySubst s x
       in let k' = applySubst s k
           in case lookup env k' of
                Just (Cont y l) -> inst l >>= simp census env (extendSubst s y x')
                _ -> pure $ Continue k' x'
    Apply f k _ xs ->
      -- traceSimp p $
      let f' = applySubst s f
          k' = applySubst s k
          xs' = map (applySubst s) xs
       in case lookup env f' of
            Just (Fn k1 _ xs1 l) ->
              inst l >>= simp census env (extendSubst (extendSubsts s (zip xs1 xs')) k1 k')
            _ -> pure $ Apply f' k' Nothing xs'
    LetPrim n op ns t ->
      let fallback = simp census env s t
       in if count census n == 0
            then fallback
            else
              let ns' = map (applySubst s) ns
               in case (op, mapM (lookup env) ns') of
                    (Primitive.EQ, Just [I32 a, I32 b]) ->
                      let v = if a == b then I32 1 else I32 0 in LetVal n v <$> simp census (addEnv env n v) s t
                    (Add2, Just [I32 a, I32 b]) ->
                      let v = I32 $ a + b in LetVal n v <$> simp census (addEnv env n v) s t
                    _ -> LetPrim n op ns' <$> fallback
    -- PushHdl eff f e ->
    --   let f' = applySubst s f
    --       e' = simp census env s e
    --    in PushHdl eff f' <$> e'
    -- PopHdl eff e ->
    --   PopHdl eff <$> simp census env s e
    Halt n ->
      pure $
        Halt (applySubst s n)
    x -> pure x

simplify' :: Term -> CompEnv Term
simplify' p = simp census Map.empty Map.empty p
  where
    census = usage p

simplify :: Term -> CompEnv Term
simplify t = do
  t' <- simplify' t
  if t' == t then pure t' else simplify t'
