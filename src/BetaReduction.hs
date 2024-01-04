{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BetaReduction where

import CPS
import Control.Lens (transformOn)
import Control.Lens.Plated (rewriteM, transform, transformM, universe)
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Util (free)
import Data.Maybe (fromMaybe)

-- reduction :: Term -> Term
-- reduction a = transform f a
--   where
-- 		f (LetCont k x t c) | k `elem` free c -> LetCont k x t

replace :: Value -> Value -> Term -> Term
replace s t = transformOn value f
  where
    f x
      | x == s = t
      | otherwise = x

-- replace :: Name -> Name -> Term -> Term
-- replace s t = replaceValue (Var s) (Var t)

type Bindings = Map.Map Name Term

collect :: Term -> Bindings
collect = Map.fromList . concatMap f . universe
  where
    f t@(LetVal n _ _) = [(n, t)]
    f t@(LetCont n _ _ _) = [(n, t)]
    f t@(LetPrim n _ _ _) = [(n, t)]
    f t@(LetSel n _ _ _) = [(n, t)]
    f _ = []

reduce :: Bindings -> Term -> Term
reduce bs = transform g
  where
    -- beta cont
    g t@(Continue (Var k) y) =
      case Map.lookup k bs of
        Just (LetCont _ x c _) -> replace (Var x) y c
        _ -> t
    -- beta fun
    g t@(Apply (Var f) j y) =
      case Map.lookup f bs of
        Just (LetVal _ (Fn k x c) _) -> replace (Var x) y . replace (Var k) j $ c
        _ -> t
    -- beta pair
    g t = t

bindings :: Term -> [(Name, Term)]
bindings t@(LetVal n _ _) = [(n, t)]
bindings t@(LetCont n _ _ _) = [(n, t)]
bindings t@(LetPrim n _ _ _) = [(n, t)]
bindings t@(LetSel n _ _ _) = [(n, t)]
bindings _ = []

reduceM :: Term -> State Bindings Term
reduceM = rewriteM g
  where
    g t = do
       
      bs <- get
      let r = case t of
            -- beta cont
            (Continue (Var k) y) -> do
              (LetCont _ x c _) <- Map.lookup k bs
              pure $ replace (Var x) y c
            -- beta fun
            (Apply (Var f) j y) -> do
              (LetVal _ (Fn k x c) _) <- Map.lookup f bs
              pure . replace (Var x) y . replace (Var k) j $ c
            -- beta pair
            (LetSel y i (Var x) _) -> do
              (LetVal _ (Tuple elems) c) <- Map.lookup x bs
              let xi = elems !! i
              pure $ replace (Var y) xi c
            _ -> Nothing
      let new = Map.fromList (maybe [] bindings r)
      put (new `Map.union` bs)
      pure r

-- reduction :: Term -> Term
-- -- beta cont
-- reduction (LetCont k x c (Continue (Var k') (Var y)) | k == k'
--        = LetCont k x c (replace x y c)
-- -- beta fun
-- reduction (LetVal f (Fn k x c) (Apply (Var f') (Var j) (Var y))) | f == f'
--        = LetVal f (Fn k x c) (replace k j . replace x y $ c)
-- -- beta case
-- reduction (LetVal x )
-- reduction _ = error ""
