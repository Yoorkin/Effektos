module BetaReduction where

import CPS
import Control.Lens (transformOn)
import Control.Lens.Plated (universe,transform)
import qualified Data.Map as Map
import Util (free)

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
