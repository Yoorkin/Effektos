{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}

module BetaReduction(reduce) where

import CPS
import Control.Lens (transformOn)
import Control.Lens.Plated (rewriteM, universe)
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Util (var)
import Data.Maybe (fromMaybe)


replace :: Name -> Name -> Term -> Term
replace s t = transformOn var f
  where
    f x
      | x == s = t
      | otherwise = x

type Bindings = Map.Map Name Term

bindings :: Term -> [(Name, Term)]
bindings t = case t of
         (LetVal n _ _) -> [(n, t)]
         (LetSel n _ _ _) -> [(n, t)]
         (LetCont n _ _ _) -> [(n, t)]
         (LetFns fns _) -> map ((,t) . fst) fns
         (LetPrim n _ _ _) -> [(n, t)]
         _ -> []

collect :: Term -> Bindings
collect t = Map.fromList (concatMap bindings $ universe t)

reduceM :: Term -> State Bindings Term
reduceM = rewriteM g
  where
    g t = do
      bs <- get
      let r = case t of
            -- beta cont / eta cont
            (Continue k y) -> do
              (LetCont _ x c _) <- Map.lookup k bs
              pure $ replace x y c
            -- beta fun
            (Apply f j y) -> do
              (LetVal _ (Fn k x c) _) <- Map.lookup f bs
              pure . replace x y . replace k j $ c
            -- beta pair
            (LetSel y i x c) -> do
              (LetVal _ (Tuple elems) _) <- Map.lookup x bs
              let xi = elems !! i
              pure $ replace y xi c
            _ -> Nothing
      let new = Map.fromList . bindings $ fromMaybe t r
      put (new `Map.union` bs)
      pure r

reduce :: Term -> Term
reduce t = evalState (reduceM t) (collect t)

