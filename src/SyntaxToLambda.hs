{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module SyntaxToLambda where

import Constant
import Control.Monad.State.Lazy
import qualified Lambda as L
import Syntax as S

constToInt :: Constant -> Int
constToInt (Integer i) = i
constToInt (Boolean b) = if b then 1 else 0
constToInt Unit = 0

fresh :: String -> State Int String
fresh n = state (\i -> (n ++ show i, i + 1))

transExpr :: Expr -> State Int L.Expr
transExpr expr = case expr of
  (Var n) -> pure $ L.Var n
  (Fun b e) -> L.Abs b <$> transExpr e
  (App f a) -> L.App <$> transExpr f <*> transExpr a
  (Let b e m) -> L.Let b <$> transExpr e <*> transExpr m
  (Fix bs fns e) ->
    let f (b, e') = (b,) <$> transExpr e' in L.Fix bs <$> mapM f fns <*> transExpr e
  (If c y n) -> do
    y' <- transExpr y
    n' <- transExpr n
    L.Switch <$> transExpr c <*> pure [(1, y'), (0, n')] <*> pure Nothing
  (Match e cs es) -> do
    es' <- mapM transExpr es
    L.Switch <$> transExpr e <*> pure (zip (map constToInt cs) es') <*> pure Nothing
  (Tuple xs) -> L.Tuple <$> mapM transExpr xs
  (Prim op es) -> L.PrimOp op <$> mapM transExpr es
  (Anno e _) -> transExpr e
  (Const c) -> pure $ L.Const c
  (Sequence e1 e2) -> L.Let <$> fresh "seq" <*> transExpr e1 <*> transExpr e2
  Hole -> error ""
  (Handle e hds) ->
    let f (a, b, c, m) = (a,b,c,) <$> transExpr m in L.Handle <$> transExpr e <*> mapM f hds
  (Resume k a) -> L.Resume <$> transExpr k <*> transExpr a

transProg :: Program -> L.Expr
transProg (Program e) = evalState (transExpr e) 0