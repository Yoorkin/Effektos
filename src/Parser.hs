module Parser where

import Lambda
import Sexp (parse, Sexp (..))


parse :: String -> Expr
parse = ofSexp . Sexp.parse


ofSexp :: Sexp -> Expr
ofSexp (SAtom c) | all (map isDigit c) = Constant (Integer $ readInt c)
