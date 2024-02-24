{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SyntaxToLambda (transProg) where

import CompileEnv
import Constant
import qualified Lambda as L
import Syntax as S
import qualified PatternMatch

constToInt :: Constant -> Int
constToInt x =
  case x of
    (Integer i) -> i
    (Boolean b) -> if b then 1 else 0
    (String _) -> error ""
    Unit -> 0

transProg :: Program -> CompEnv L.Expr
transProg (Program defs expr) = translExpr defs [] expr

translExpr :: [Definition] -> [String] -> Expr -> CompEnv L.Expr
translExpr defs effects expr =
  let go = translExpr defs effects
   in case expr of
        (Var n) -> pure $ L.Var (synName n)
        (Fun (PatVar b) e) -> L.Abs (synName b) <$> go e
        (App f a) ->
          case f of
            (Var n) | n `elem` effects -> go (Raise n a)
            _ -> L.App <$> go f <*> go a
        (Let (PatVar b) e m) -> L.Let (synName b) <$> go e <*> go m
        (Fix bs fns e) ->
          let f (PatVar b, e') = (,) (synName b) <$> go e'
              bs' = map synName bs
           in L.Fix bs' <$> mapM f fns <*> go e
        (If c y n) -> do
          y' <- go y
          n' <- go n
          L.Switch <$> go c <*> pure [(1, y'), (0, n')] <*> pure Nothing
        (Match e ps es) -> do
          let e = PatternMatch.transl expr defs
          es' <- mapM go es
          L.Switch <$> go e <*> pure (map (1,) es') <*> pure Nothing
        (Tuple xs) -> L.Tuple <$> mapM go xs
        (Prim op es) -> L.PrimOp op <$> mapM go es
        (Anno e _) -> go e
        (Const c) -> pure $ L.Const c
        (Sequence e1 e2) -> L.Let <$> fresh <*> go e1 <*> go e2
        Hole -> error ""
        -- (Handle e hds) ->
        --   let f (eff, x, k, m) = (eff,synName x,synName k,) <$> go m
        --    in L.Handle <$> go e <*> mapM f hds
        (Raise eff x) -> L.Raise eff <$> go x
        (Resume k a) -> L.Resume <$> go k <*> go a
        (EffectDef effs e) -> translExpr defs effs e
        _ -> error $ show expr

-- TODO: the translation of pattern matching needs some information:
--  1. The arity of each data constructor.
--  2. The data type to which each data constructor belongs.
--  3. All constructors of a given data typ.
--
-- additionally, to distinguish between `PatVar id` and `PatConstr id []` for `id`:
--  1. Provide all constructors name
--
-- possible solution:
--
-- type Arity = Int
-- type DataType = String
-- data ConstrInfo = Constructor Arity DataType
-- data DataInfo = DataInfo ConstrInfo
-- dataMap :: Map DataType DataInfo
-- constrMap :: Map Constr ConstrInfo
