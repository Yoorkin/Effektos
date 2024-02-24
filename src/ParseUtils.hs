{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ParseUtils where

import Lexer
import Primitive as Op
import Syntax

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show (take 10 tokens)

recBindings :: [(Binder, Expr)] -> Expr -> Expr
recBindings xs = Fix bs fns
  where
    f (b, Fun p e) = (b, (p, e))
    f _ = error "not a valid rec function"
    (bs, fns) = unzip (map f xs)

selectPrimOp :: Token -> Primitive
selectPrimOp (Token _ _ _ (Symbol x)) =
  case x of
    "+" -> Add2
    "-" -> Sub2
    "*" -> Mul
    "/" -> Div
    ">" -> Op.GT
    "<" -> Op.LT
    ">=" -> GE
    "<=" -> LE
    "==" -> Op.EQ
    "<>" -> NE
    _ -> error $ show x
selectPrimOp _ = error ""
