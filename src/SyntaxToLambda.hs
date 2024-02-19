{-# LANGUAGE TupleSections #-}

module SyntaxToLambda (transProg) where

import CompileEnv
import Constant
import qualified Lambda as L
import Syntax as S

constToInt :: Constant -> Int
constToInt (Integer i) = i
constToInt (Boolean b) = if b then 1 else 0
constToInt Unit = 0

transProg :: Program -> CompEnv L.Expr
transProg (Program e) = transExpr e

translExpr :: [String] -> Expr -> CompEnv L.Expr
translExpr effects expr = 
  let go = translExpr effects in
  case expr of
    (Var n) -> pure $ L.Var (synName n)
    (Fun b e) -> L.Abs (synName b) <$> go e
    (App f a) -> 
      case f of
        (Var n) | n `elem` effects -> go (Raise n a)
        _ -> L.App <$> go f <*> go a
    (Let b e m) -> L.Let (synName b) <$> go e <*> go m
    (Fix bs fns e) ->
      let f (b, e') = (,) (synName b) <$> go e'
          bs' = map synName bs
       in L.Fix bs' <$> mapM f fns <*> go e
    (If c y n) -> do
      y' <- go y
      n' <- go n
      L.Switch <$> go c <*> pure [(1, y'), (0, n')] <*> pure Nothing
    (Match e cs es) -> do
      es' <- mapM go es
      L.Switch <$> go e <*> pure (zip (map constToInt cs) es') <*> pure Nothing
    (Tuple xs) -> L.Tuple <$> mapM go xs
    (Prim op es) -> L.PrimOp op <$> mapM go es
    (Anno e _) -> go e
    (Const c) -> pure $ L.Const c
    (Sequence e1 e2) -> L.Let <$> fresh <*> go e1 <*> go e2
    Hole -> error ""
    (Handle e hds) ->
      let f (eff, x, k, m) = (eff,synName x,synName k,) <$> go m
       in L.Handle <$> go e <*> mapM f hds
    (Raise eff x) -> L.Raise eff <$> go x
    (Resume k a) -> L.Resume <$> go k <*> go a
    (EffectDef effs e) -> translExpr effs e

transExpr :: Expr -> CompEnv L.Expr
transExpr = translExpr []
