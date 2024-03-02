{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SyntaxToLambda (transProg) where

import CompileEnv
import Constant
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import DefinitionInfo
import qualified Lambda as L
import qualified PatternMatch
import Syntax as S
import qualified Data.Bifunctor
import Data.Bifunctor (Bifunctor(first))

constToInt :: Constant -> Int
constToInt x =
  case x of
    (Integer i) -> i
    (Boolean b) -> if b then 1 else 0
    (String _) -> error ""
    Unit -> 0

transProg :: Program -> CompEnv L.Expr
transProg (Program defs expr) = translExpr datatypes constrs effects expr
  where
    (datatypes, constrs, effects) = preprocessDefs defs

preprocessDefs :: [Definition] -> (Map String DataTypeInfo, Map String ConstrInfo, Map String EffectInfo)
preprocessDefs = aux initialDataTypes initialConstrs []
  where
    aux datatypes constrs effects [] =
      (Map.fromList datatypes, Map.fromList constrs, Map.fromList effects)
    aux datatypes constrs effects (def : defs) =
      case def of
        (Effect name _) -> 
          let eff = EffectInfo 1 in
          aux datatypes constrs ((name, eff) : effects) defs
        (Data typename pairs) ->
          let constrs' = map (mkConstrInfo typename) pairs
              datatype = (typename, DataTypeInfo (map fst pairs))
           in aux (datatype : datatypes) (constrs' ++ constrs) effects defs
    mkConstrInfo typename (constr, args) = (constr, ConstrInfo (length args) typename)
    initialDataTypes =
      [ ("Unit", DataTypeInfo ["()"])
      ]
    initialConstrs =
      [ ("()", ConstrInfo 0 "Unit")
      ]

preprocessPattern :: Map String ConstrInfo -> Pattern -> Pattern
preprocessPattern constrs pat =
   let go = preprocessPattern constrs in
      case pat of
        (PatConstr c []) | c `Map.notMember` constrs -> PatVar c
        (PatVar {}) -> pat
        (PatConstr c qs) -> PatConstr c (map go qs)
        (PatConstant {}) -> pat
        (PatTuple qs) -> PatTuple (map go qs)
        (PatOr q1 q2) -> PatOr (go q1) (go q2)
        PatWildCard -> pat


translExpr ::
  Map String DataTypeInfo ->
  Map String ConstrInfo ->
  Map String EffectInfo ->
  Expr ->
  CompEnv L.Expr
translExpr datatypes constrs effects expr =
  let go = translExpr datatypes constrs effects
   in case expr of
        (Var n) -> pure $ L.Var (synName n)
        (Fun (PatConstr b []) e) | b `Map.notMember` constrs -> L.Abs (synName b) <$> go e
        (App f a) ->
          case f of
            (Var n) | n `Map.member` effects -> go (Raise n a)
            _ -> L.App <$> go f <*> go a
        (Let (PatConstr b []) e m) | b `Map.notMember` constrs -> L.Let (synName b) <$> go e <*> go m
        (Fix bs fns e) ->
          let f (PatConstr b [], e') | b `Map.notMember` constrs = (,) (synName b) <$> go e'
              bs' = map synName bs
           in L.Fix bs' <$> mapM f fns <*> go e
        (If c y n) -> do
          y' <- go y
          n' <- go n
          L.Switch <$> go c <*> pure [(Constant.Integer 1, y'), (Constant.Integer 0, n')] <*> pure Nothing
        (Match e ps es) -> do
          let ps' = map (preprocessPattern constrs) ps
          PatternMatch.transl datatypes constrs go (Match e ps' es)
        (Tuple xs) -> L.Tuple <$> mapM go xs
        (Prim op es) -> L.PrimOp op <$> mapM go es
        (Anno e _) -> go e
        (Const c) -> pure $ L.Const c
        (Sequence e1 e2) -> L.Let <$> fresh <*> go e1 <*> go e2
        Hole -> error ""
        (Handle e hds) ->
          let f (PatConstr eff [PatVar x, PatVar k], m) = (eff,synName x,synName k,) <$> go m
           in L.Handle <$> go e <*> mapM (f . first (preprocessPattern constrs)) hds
        (Raise eff x) -> L.Raise eff <$> go x
        (Resume k a) -> L.Resume <$> go k <*> go a
        _ -> error $ show expr
