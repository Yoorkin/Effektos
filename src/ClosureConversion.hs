{-# LANGUAGE LambdaCase #-}

module ClosureConversion where

import CPS
import Control.Lens (transformM)
import Control.Lens.Plated (transformOn)
import Control.Monad.State.Lazy
import Data.List ((\\))
import qualified Data.Map.Lazy as Map
import Util (free, var)

type Stamp = Int

rename :: Name -> Name -> Term -> Term
rename a b = transformOn var f
  where
    f x = if x == a then b else x

renames :: [Name] -> [Name] -> Term -> Term
renames (a : as) (b : bs) t = renames as bs (rename a b t)
renames [] [] t = t
renames _ _ _ = error ""

wrapProj :: Name -> [Name] -> Term -> Term
wrapProj env closure = f (zip [1 ..] closure)
  where
    f ((i, v) : xs) acc = f xs (LetSel v i env acc)
    f [] acc = acc

transClosure :: Stamp -> Term -> Term
transClosure stamp t = evalState (transformM f t) stamp
  where
    f (LetVal n (Fn k _ xs l) m) = do
      let fvars = free l \\ (k : xs)
      nfvars <- mapM fresh fvars
      let code = n ++ "_code"
      let env = n ++ "_env"
      let l' = wrapProj env nfvars (renames nfvars fvars l)
      pure $
        LetVal
          code
          (Fn k (Just env) xs l')
          (LetVal n (Tuple (code : fvars)) m)
    f (Apply g k _ xs) = do
      fname <- fresh g
      let closure = g
      pure $ LetSel fname 0 closure (Apply fname k (Just closure) xs)
    f (LetCont k _ x l m) = do
      let fvars = free l \\ [k, x]
      nfvars <- mapM fresh fvars
      let code = k ++ "_code"
      let env = k ++ "_env"
      let l' = wrapProj env nfvars (renames fvars nfvars l)
      pure $ LetCont code (Just env) x l' (LetVal k (Tuple (code : fvars)) m)
    f (Continue k _ x) = do
      kname <- fresh k
      let env = k
      pure $ LetSel kname 0 env (Continue kname (Just env) x)
    f (LetFns fns l) =
      let collectVars fv nfv = \case
            ((_, Fn k Nothing xs m) : fns') -> do
              let fv' = free m \\ (k : xs)
              nfv' <- mapM fresh fv'
              collectVars fv' nfv' fns'
            [] -> pure (reverse fv, reverse nfv)
            _ -> error ""

          transFnBody fv nfv acc = \case
            ((n, Fn k Nothing xs m) : fns') ->
              let code = k ++ "_code"
                  env = k ++ "_env"
                  -- FIX: exclude Handlers name from closure
                  m' = wrapProj env nfv (renames fv nfv m)
               in transFnBody fv nfv ((n, Fn code (Just env) xs m') : acc) fns'
            [] -> reverse acc
            _ -> error ""
       in do
            (fvars, nfvars) <- collectVars [] [] fns
            pure $ LetFns (transFnBody fvars nfvars [] fns) l
    f x = pure x

    fresh :: Name -> State Int Name
    fresh x = state (\i -> (x ++ "_" ++ show i, i + 1))
