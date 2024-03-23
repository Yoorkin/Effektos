module Syntax.AST
  ( Program (..),
    Anno (..),
    Binder,
    Fn,
    Expr (..),
    Pattern(..),
    Definition(..),
    Constr
  )
where

import Syntax.Constant
import Syntax.Primitive

data Program
  = Program [Definition] Expr
  deriving (Show)

type Constr = String

data Definition
  = Data String [(Constr, [Anno])]
  | Effect String Anno
  deriving (Show)

data Anno
  = AnnoVar String
  | AnnoArrow Anno Anno
  | AnnoTuple [Anno]
  | AnnoTypeConstr String [Anno]
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
  | Anno Expr Anno
  | Const Constant
  | Sequence Expr Expr
  | Hole
  | Handle Expr [(Pattern, Expr)]
  | Resume Expr Expr
  | Raise String Expr
  deriving (Show)
