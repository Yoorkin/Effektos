{-# LANGUAGE LambdaCase #-}

module Flatvm (run) where

import Data.Map.Internal.Debug (showTree)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId, traceWith)
import Flat
import Primitive

data RtValue
  = RtTuple [RtValue]
  | RtVar String
  | RtI32 Int
  | RtUnit
  | RtCode Fn
  deriving (Show)

type Env = Map.Map String RtValue

evalLiteral :: Env -> Value -> RtValue
evalLiteral env = \case
  Unit -> RtUnit
  I32 i -> RtI32 i
  Var n -> RtVar n
  Proj i n -> case Map.lookup n env of
    Just (RtTuple xs) | length xs > i -> xs !! i
    _ -> error $ "invalid projection for " ++ n
  Tuple xs -> case mapM (`Map.lookup` env) xs of
    Just vs -> RtTuple vs
    Nothing -> error "invalid tuple"
  PrimOp op ns ->
    case mapM (`Map.lookup` env) ns of
      Just [RtI32 a, RtI32 b] ->
        case op of
          Add2 -> RtI32 (a + b)
          Sub2 -> RtI32 (a - b)
          _ -> error "invalid operation"
      _ -> error "invalid operands"

evalBindings :: Env -> [Binding] -> Env
evalBindings env [] = env
evalBindings env (Binding n v : bs) =
  let env' = Map.insert n (evalLiteral env v) env
   in evalBindings env' bs

eval :: Env -> Expr -> RtValue
eval env e@(Apply f args) =
  case Map.lookup f env of
    (Just (RtCode (Fn _ ps bs e))) ->
      case mapM (`Map.lookup` env) args of
        Nothing -> error "invalid arguments"
        Just vs ->
          let xs = Map.fromList (zip ps vs)
              env' = evalBindings (xs `Map.union` env) bs
           in eval env' e
    _ -> error "invalid function"
-- eval env (Switch n es) =
--   case Map.lookup n env of
--     (Just (RtI32 i)) -> eval env (es !! i)
--     _ -> error "invalid condition"
eval env (Exit n) = fromJust $ Map.lookup n env

run :: Program -> RtValue
run (Program m fns) = eval env (Apply "main" [])
  where
    env = Map.fromList (map g (m : fns))
    g f@(Fn n _ _ _) = (n, RtCode f)

debugEval env expr result =
  "============================== eval ======================="
    ++ showTree env
    ++ "----------------------"
    ++ show expr
    ++ "----------------------"
    ++ show result
