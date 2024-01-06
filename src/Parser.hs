{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser where

import Data.Char (isDigit)
import Lambda
import Sexp (Sexp (..), parse)

parse :: String -> Expr
parse = ofSexp . Sexp.parse

ofSexp :: Sexp -> Expr
ofSexp (SAtom c)
  | all isDigit c = Const (Integer $ read c)
  | otherwise = Var c
ofSexp (SList xs) = case xs of
  [SAtom "lambda", SAtom n, e] -> Abs n (ofSexp e)
  [SAtom "let", SAtom n, e1, e2] -> Let n (ofSexp e1) (ofSexp e2)
  [SAtom "fix", SList fns, e] ->
    let f (SList [SAtom n, SList [SAtom a, e']] : xs') acc = f xs' ((n, (a, ofSexp e')) : acc)
        f [] acc = reverse acc
     in Fix (f fns []) (ofSexp e)
  (SAtom "tuple" : elems) -> Tuple (map ofSexp elems)
  [SAtom "select", SAtom i, e] -> Select (read i) (ofSexp e)
  (SAtom "switch" : c : bs) ->
    let f ((SList [SAtom i, e2]) : bs') acc fallback
          | i == "default" = f bs' acc (Just $ ofSexp e2)
          | otherwise = f bs' ((read i, ofSexp e2) : acc) fallback
        f [] acc fallback = (reverse acc, fallback)
     in let (cases, fallback) = f bs [] Nothing
         in Switch (ofSexp c) cases fallback
  [SAtom "+",a,b] -> PrimOp Add2 [ofSexp a, ofSexp b]
  [SAtom "-",a,b] -> PrimOp Sub2 [ofSexp a, ofSexp b]
  [SAtom "abort"] -> PrimOp Abort []
  [x] -> ofSexp x
  f : args ->
    let g (e : es) f' = g es (App f' (ofSexp e))
        g [] f' = f'
     in g args (ofSexp f)

