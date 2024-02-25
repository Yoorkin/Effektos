module Syntax
  ( Program (..),
    Type (..),
    Binder,
    Fn,
    Expr (..),
    Pattern(..),
    Definition(..),
    Constr
  )
where

import Constant
import Primitive

data Program
  = Program [Definition] Expr
  deriving (Show)

type Constr = String

data Definition
  = Data String [(Constr, [Type])]
  | Effect String Type
  deriving (Show)

data Type
  = TypeVar String
  | TypeTuple [Type]
  | TypeArrow Type Type
  deriving (Show)

type Binder = String

type Fn = (Pattern, Expr)

data Pattern
  = PatVar String
  | PatConstr Constr [Pattern]
  | PatConstant Constant
  | PatTuple [Pattern]
  | PatOr Pattern Pattern
  | PatWildCard
  deriving (Show)

data Expr
  = Var String
  | Fun Pattern Expr
  | App Expr Expr
  | Let Pattern Expr Expr
  | Fix [Binder] [Fn] Expr
  | If Expr Expr Expr
  | Match Expr [Pattern] [Expr]
  | Tuple [Expr]
  | Prim Primitive [Expr]
  | Anno Expr Type
  | Const Constant
  | Sequence Expr Expr
  | Hole
  | Handle Expr [(Pattern, Expr)]
  | Resume Expr Expr
  | Raise String Expr
  deriving (Show)
