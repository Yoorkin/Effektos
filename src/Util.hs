{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Util where

import CPS
import Control.Comonad.Store (pos)
import Control.Lens ( Traversal', Plated(..) )
import Control.Lens.Plated ( universe, contextsOn )
import Data.List ((\\),nub)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe, maybeToList)
import Control.Lens.Traversal (mapMOf)
import Control.Lens.Fold (toListOf)
import Data.Foldable (toList)
import CompileEnv

bound :: Term -> [Name]
bound e = nub $ concatMap f (universe e)
  where
    f (LetVal n (Fn k _ xs _) _) = n:k:xs
    f (LetVal n _ _) = [n]
    f (LetSel n _ _ _) = [n]
    f (LetCont n1 env n2 _ _) = toList env ++ [n1,n2]
    f (LetFns fns _) = map fst fns
    f (LetPrim n _ _ _) = [n]
    f _ = []

occur :: Term -> [Name]
occur = nub . toListOf var

free :: Term -> [Name]
free e = occur e \\ bound e

def :: Term -> [Name]
def t = concatMap f $ universe t
  where
    f (LetVal n _ _) = [n]
    f (LetSel n _ _ _) = [n]
    f (LetCont n1 env n2 _ _) = toList env ++ [n1,n2]
    f (LetFns fns _) = map fst fns
    f (LetPrim n _ _ _) = [n]
    f _ = []

used :: Term -> [Name]
used = concatMap f . universe
  where
    f (LetVal _ (Var n) _) = [n]
    f (LetVal {}) = []
    f (LetSel _ _ n _) = [n]
    f (LetCont {}) = []
    f (LetFns {}) = []
    f (Continue n1 env n2) = toList env ++ [n1, n2]
    f (Apply n1 n2 env ns) = toList env ++ n1 : n2 : ns
    f (LetPrim _ _ ns _) = ns
    f (Switch n _ _) = [n]
    f (Halt n) = [n]
    f (Handle _ n2 _) = [n2]
    f (Raise n1 n2 mn ns) = maybeToList mn ++ n1:n2:ns


usage :: Term -> Map.Map Name Int
usage = f Map.empty . map pos . contextsOn var
  where
    f count = \case
      (x : xs) -> f (Map.alter (Just . (+ 1) . fromMaybe (-1)) x count) xs
      [] -> count

usageCount :: Name -> Term -> Int
usageCount n = length . filter (== n) . map pos . contextsOn var

visit :: (Monad m, Plated a) => (a -> m a) -> (a -> m a) -> a -> m a
visit f g x = g =<< mapMOf plate (visit f g) =<< f x

var :: Traversal' Term Name
var f = goExpr
  where
    goExpr = \case
      (LetVal n v t) -> LetVal <$> f n <*> goValue v <*> goExpr t
      (LetSel n i n2 t) -> LetSel <$> f n <*> pure i <*> f n2 <*> goExpr t
      (LetCont n1 mn n2 t1 t2) -> LetCont <$> f n1 <*> traverse f mn <*> f n2 <*> goExpr t1 <*> goExpr t2
      (LetFns fns t1) -> LetFns <$> traverse (\(a, b) -> (,) <$> f a <*> goValue b) fns <*> goExpr t1
      (Continue n1 mn n2) -> Continue <$> f n1 <*> traverse f mn <*> f n2
      (Apply n1 n2 env n3) -> Apply <$> f n1 <*> f n2 <*> traverse f env <*> traverse f n3
      (LetPrim n p ns t) -> LetPrim <$> f n <*> pure p <*> traverse f ns <*> goExpr t
      (Switch n ix ts) -> Switch <$> f n <*> pure ix <*> traverse goExpr ts
      (Halt v) -> Halt <$> f v
      (Handle h fn t) -> Handle h <$> f fn <*> goExpr t
      (Raise h k env x) -> Raise <$> f h <*> f k <*> traverse f env <*> traverse f x
    goValue x = case x of
      (Var n) -> Var <$> f n
      (I32 _) -> pure x
      Unit -> pure x
      (Tuple xs) -> Tuple <$> traverse f xs
      (Cont mn n t) -> Cont <$> traverse f mn <*> f n <*> goExpr t
      (Fn n1 env n2 t) -> Fn <$> f n1 <*> traverse f env <*> traverse f n2 <*> goExpr t
