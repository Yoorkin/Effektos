module Typing.Typedtree where

import Syntax.Constant
import Syntax.Primitive
import Typing.Symbol

type Constr = String

type Binder = String

type Fn = (Pattern, Expr)

data Pattern
  = PatVar Type String
  | PatConstr Type Constr [Pattern]
  | PatConstant Type Constant
  | PatTuple Type [Pattern]
  | PatOr Type Pattern Pattern
  | PatWildCard Type
  deriving (Show)

data Expr
  = Var Type String
  | Fun Type Pattern Expr
  | App Type Expr Expr
  | Let Type Pattern Expr Expr
  | Fix Type [Binder] [Fn] Expr
  | If Type Expr Expr Expr
  | Match Type Expr [Pattern] [Expr]
  | Tuple Type [Expr]
  | Prim Type Primitive [Expr]
  | Const Type Constant
  | Sequence Type Expr Expr
  | Hole Type
  | Handle Type Expr [(Pattern, Expr)]
  | Resume Type Expr Expr
  | Raise Type String Expr
  deriving (Show)

typeOfPat :: Pattern -> Type
typeOfPat (PatVar ty _) = ty

typeOfExpr :: Expr -> Type
typeOfExpr (Var ty _) = ty
