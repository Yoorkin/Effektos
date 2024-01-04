{-# LANGUAGE LambdaCase #-}

module DCE(eliminate) where

import CPS
import Util(bound,occur)
import Control.Lens (transform)
import Data.List ((\\))

unused :: Term -> [Name]
unused e = bound e \\ occur e

eliminate :: Term -> Term
eliminate a = if a == b then b else eliminate b
  where
    isDead x = x `elem` unused a
    b = transform f a
    f (LetVal n _ t) | isDead n = t
    f (LetSel n _ _ t) | isDead n = t
    f (LetCont _ n t _) | isDead n = t 
    f (LetCont k _ _ t) | isDead k = t
    f (LetFns fns t) = LetFns (filter (not . isDead . fst) fns) t
    f x = x


