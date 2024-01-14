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
    f x = pure x

    fresh x = state (\i -> (x ++ "_" ++ show i, i + 1))
