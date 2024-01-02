{-# LANGUAGE LambdaCase #-}
module Util where
import CPS
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Control.Lens (transform)
import Control.Lens.Combinators (universeOn)
import Control.Lens.Plated (universe, Plated)
import Control.Lens.Combinators (mapMOf)
import Control.Lens.Combinators (plate)

bound :: Term -> [Name]
bound e = concatMap f (universe e)
  where
    f (LetVal n _ _) = [n]
    f (LetSel n _ _ _) = [n]
    f (LetCont n1 n2 _ _) = [n1, n2]
    f (LetFns fns _) = map fst fns
    f _ = []

occur :: Term -> [Name]
occur e =
  mapMaybe
    (\case Var n -> Just n; _ -> Nothing)
    (universeOn value e)

free :: Term -> [Name]
free e = occur e \\ bound e

visit :: (Monad m, Plated a) => (a -> m a) -> (a -> m a) -> a -> m a
visit f g x = g =<< mapMOf plate (visit f g) =<< f x

