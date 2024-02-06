{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ParseUtils where
import Lexer
import Syntax
import Primitive as Op

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show (take 10 tokens)

recBindings :: [(Binder, Expr)] -> Expr -> Expr
recBindings xs' expr' = f [] [] xs'
        where f bs fns ((b, Fun args expr):xs) = f (b:bs) ((args,expr):fns) xs
              f bs fns [] = Fix (reverse bs) (reverse fns) expr'  
              f _ _ (x:_) = error $ "invalid fix expression: " ++ show x

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
        "=" -> Op.EQ
        "<>" -> NE
        _ -> error $ show x
selectPrimOp _ = error ""
