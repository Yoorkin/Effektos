{-# LANGUAGE MonoLocalBinds #-}

module Uniquify (uniquifyTerm) where

import qualified CPS
import CompileEnv
import Control.Lens
import Control.Monad.State.Lazy as State
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Bifunctor
import qualified Data.Map as Map
import Data.Maybe
import Lambda
import Util (def, var)

visit :: (Monad m, Plated a) => (a -> m a) -> (a -> m a) -> a -> m a
visit f g x = g =<< mapMOf plate (visit f g) =<< f x

type Env = Map.Map String [String]

push :: (Ord k) => k -> v -> Map.Map k [v] -> Map.Map k [v]
push k v = Map.alter (Just . (:) v . fromMaybe []) k

pop :: (Ord k) => k -> Map.Map k [v] -> Map.Map k [v]
pop = Map.adjust tail

top :: (Ord k) => k -> Map.Map k [v] -> Maybe v
top k mp = case fromMaybe [] $ Map.lookup k mp of [] -> Nothing; x : _ -> Just x

enter, exit :: Expr -> CompEnvT (State Env) Expr
enter (Abs old e) =
  do
    mp <- lift get
    new <- freshWithBase old
    lift $ put (push old new mp)
    pure $ Abs old e
enter (Let old e1 e2) =
  do
    mp <- lift get
    new <- freshWithBase old
    lift $ put (push old new mp)
    pure $ Let old e1 e2
enter (Var old) =
  do
    mp <- lift get
    pure $ Var (fromMaybe old (top old mp))
enter (Handle e hds) = g hds
  where
    g [] = do pure $ Handle e hds
    g ((_, x, k, _) : hds') = do
      k' <- freshWithBase k
      x' <- freshWithBase x
      lift $ modify (push k k' . push x x')
      g hds'
enter (Fix ns fs e) = g (zip ns fs)
  where
    g [] = do pure $ Fix ns fs e
    g ((n, (x, _)) : fs') = do
      n' <- freshWithBase n
      x' <- freshWithBase x
      lift $ modify (push n n' . push x x')
      g fs'
enter x = pure x
exit (Abs old e) = do
  mp <- lift get
  let new = fromJust $ top old mp
  lift $ modify (pop old)
  pure $ Abs new e
exit (Let old e1 e2) = do
  mp <- lift get
  let new = fromJust $ top old mp
  lift $ modify (pop old)
  pure $ Let new e1 e2
exit (Handle c hds) = do
  mp <- lift get
  let g n = fromJust $ top n mp
  let f (h, x, k, e) = (h, g x, g k, e)
  pure $ Handle c (map f hds)
exit (Fix ns fs e) = do
  mp <- lift get
  let g n = fromJust $ top n mp
  pure $ Fix (map g ns) (map (first g) fs) e
exit x = pure x

uniquifyTerm :: Expr -> Expr
uniquifyTerm e = evalState (evalStateT (visit enter exit e) mkCompStates) Map.empty
