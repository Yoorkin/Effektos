{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Util.Utils where

import Control.Comonad.Store (pos)
import Control.Lens (Plated (..), Traversal')
import Control.Lens.Fold (toListOf)
import Control.Lens.Plated (contextsOn, universe)
import Control.Lens.Traversal (mapMOf)
import Core.CPS
import Data.Foldable (toList)
import Data.List (nub, (\\))
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Util.CompileEnv

bound :: Term -> [Name]
bound e = nub $ concatMap f (universe e)
  where
    f (LetVal n (Fn k _ xs _) _) = n : k : xs
    f (LetVal n _ _) = [n]
    f (LetSel n _ _ _) = [n]
    f (LetCont n1 n2 _ _) = [n1, n2]
    f (LetFns fns _) = concatMap g fns
      where
        g (n, Fn k _ args _) = n : k : args
        g _ = error ""
    f (LetPrim n _ _ _) = [n]
    f _ = []

occur :: Term -> [Name]
occur = nub . toListOf var

occurCount :: Term -> Map.Map Name Int
occurCount t = f Map.empty (toListOf var t)
  where
    f count = \case
      (x : xs) -> f (Map.alter (Just . (+ 1) . fromMaybe 0) x count) xs
      [] -> count

free :: Term -> [Name]
free e = occur e \\ bound e

def :: Term -> [Name]
def t = concatMap f $ universe t
  where
    f (LetVal n _ _) = [n]
    f (LetSel n _ _ _) = [n]
    f (LetCont n1 n2 _ _) = [n1, n2]
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
    f (LetConts {}) = []
    f (Continue n1 n2) = [n1, n2]
    f (Apply n1 n2 env ns) = toList env ++ n1 : n2 : ns
    f (LetPrim _ _ ns _) = ns
    f (Switch n _ _ _) = [n]
    f (Halt n) = [n]
    f (Handle _ hdls) = map snd hdls
    f (Raise _ n ns) = n : ns

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
      (LetCont n1 n2 t1 t2) -> LetCont <$> f n1 <*> f n2 <*> goExpr t1 <*> goExpr t2
      (LetFns fns t1) -> LetFns <$> traverse (\(a, b) -> (,) <$> f a <*> goValue b) fns <*> goExpr t1
      (LetConts conts t1) -> LetFns <$> traverse (\(a, b) -> (,) <$> f a <*> goValue b) conts <*> goExpr t1
      (Continue n1 n2) -> Continue <$> f n1 <*> f n2
      (Apply n1 n2 env n3) -> Apply <$> f n1 <*> f n2 <*> traverse f env <*> traverse f n3
      (LetPrim n p ns t) -> LetPrim <$> f n <*> pure p <*> traverse f ns <*> goExpr t
      (Switch n ix ts fb) -> Switch <$> f n <*> pure ix <*> traverse goExpr ts <*> traverse goExpr fb
      (Halt v) -> Halt <$> f v
      (Handle e hdls) -> Handle <$> goExpr e <*> traverse (\(x, y) -> (x,) <$> f y) hdls
      (Raise eff k x) -> Raise eff <$> f k <*> traverse f x
    goValue x = case x of
      (Var n) -> Var <$> f n
      (I32 _) -> pure x
      Unit -> pure x
      (Tuple xs) -> Tuple <$> traverse f xs
      (Cont n t) -> Cont <$> f n <*> goExpr t
      (Fn n1 env n2 t) -> Fn <$> f n1 <*> traverse f env <*> traverse f n2 <*> goExpr t
