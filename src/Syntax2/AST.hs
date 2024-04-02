module Syntax2.AST
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
import Util.CompileEnv

data Program
  = Program [Definition] Expr
  deriving (Show)

type Constr = Name

data Definition
  = Data Name [(Constr, [Anno])]
  deriving (Show)

data Anno
  = AnnoVar Name
  | AnnoArrow Anno Anno
  | AnnoTuple [Anno]
  | AnnoTypeConstr Name [Anno]
  deriving (Show)

type Binder = Name

type Fn = (Pattern, Expr)

data Pattern
  = PatVar Name
  | PatConstr Constr [Pattern]
  | PatConstant Constant
  | PatTuple [Pattern]
  | PatOr Pattern Pattern
  | PatWildCard
  deriving (Show)

data Expr
  = Var Name
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
  deriving (Show)


