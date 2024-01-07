{-# LANGUAGE MonoLocalBinds #-}

module Uniquify (uniquify) where

import qualified CPS
import Control.Lens
import Control.Monad.State.Lazy as State
import qualified Data.Map as Map
import Data.Maybe
import Lambda
import Util (def, var)

visit :: (Monad m, Plated a) => (a -> m a) -> (a -> m a) -> a -> m a
visit f g x = g =<< mapMOf plate (visit f g) =<< f x

type Env = (Int, Map.Map String [String])

push :: (Ord k) => k -> v -> Map.Map k [v] -> Map.Map k [v]
push k v = Map.alter (Just . (:) v . fromMaybe []) k

pop :: (Ord k) => k -> Map.Map k [v] -> Map.Map k [v]
pop = Map.adjust tail

top :: (Ord k) => k -> Map.Map k [v] -> Maybe v
top k mp = case fromMaybe [] $ Map.lookup k mp of [] -> Nothing; x : _ -> Just x

enter, exit :: Expr -> State Env Expr
enter (Abs old e) =
  state $
    \(i, mp) ->
      let new = old ++ show i
          s = (i + 1, push old new mp)
       in (Abs old e, s)
enter (Let old e1 e2) =
  state $ \(i, mp) ->
    let new = old ++ show i
        s = (i + 1, push old new mp)
     in (Let old e1 e2, s)
enter (Var old) = gets $ \(_, mp) -> Var (fromMaybe old (top old mp))
enter x = pure x
exit (Abs old e) =
  state $
    \(i, mp) ->
      let new = fromJust $ top old mp
          s = (i, pop old mp)
       in (Abs new e, s)
exit (Let old e1 e2) =
  state $
    \(i, mp) ->
      let new = fromJust $ top old mp
          s = (i, pop old mp)
       in (Let new e1 e2, s)
exit x = pure x

uniquify :: Expr -> Expr
uniquify e = evalState (visit enter exit e) (0, Map.empty)

