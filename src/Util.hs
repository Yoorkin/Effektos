{-# LANGUAGE LambdaCase #-}

module Util where

import CPS
import Control.Lens (Traversal')
import Control.Lens.Combinators (mapMOf, plate, universeOn)
import Control.Lens.Plated (Plated, universe)
import Data.List ((\\))
import Control.Lens (contextsOn)
import Control.Comonad.Store (pos)

bound :: Term -> [Name]
bound e = concatMap f (universe e)
  where
    f (LetVal n _ _) = [n]
    f (LetSel n _ _ _) = [n]
    f (LetCont n1 n2 _ _) = [n1, n2]
    f (LetFns fns _) = map fst fns
    f _ = []

occur :: Term -> [Name]
occur = universeOn var

free :: Term -> [Name]
free e = occur e \\ bound e

usageCount :: Name -> Term -> Int
usageCount n = length . filter (==n) . map pos . contextsOn var 

visit :: (Monad m, Plated a) => (a -> m a) -> (a -> m a) -> a -> m a
visit f g x = g =<< mapMOf plate (visit f g) =<< f x

var :: Traversal' Term Name
var f = goExpr
  where
    goExpr = \case
      (LetVal n v t) -> LetVal n <$> goValue v <*> goExpr t
      (LetSel n i n2 t) -> LetSel n i <$> f n2 <*> goExpr t
      (LetCont n1 n2 t1 t2) -> LetCont n1 n2 <$> goExpr t1 <*> goExpr t2
      (LetFns fns t1) -> LetFns <$> traverse (\(a, b) -> (,) a <$> goValue b) fns <*> goExpr t1
      (Continue n1 n2) -> Continue <$> f n1 <*> f n2
      (Apply n1 n2 n3) -> Apply <$> f n1 <*> f n2 <*> f n3
      (LetPrim n p ns t) -> LetPrim n p <$> traverse f ns <*> goExpr t
      (Switch n ts) -> Switch <$> f n <*> traverse goExpr ts
      (Halt v) -> Halt <$> f v
    goValue x = case x of
      (Var n) -> Var <$> f n
      (I32 _) -> pure x
      Unit -> pure x
      (Tuple xs) -> Tuple <$> traverse f xs
      (Cont n t) -> Cont n <$> goExpr t
      (Fn n1 n2 t) -> Fn n1 n2 <$> goExpr t
