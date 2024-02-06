module Syntax(Program(..), Type(..), Binder, Fn, Expr(..)) where
import Primitive
import Constant

data Program
  = Program Expr
  deriving (Show)

data Type
  = TypeVar String
  | TypeTuple [Type]
  | TypeArrow Type Type
  deriving (Show)

type Binder = String

type Fn = (Binder, Expr)

data Expr
  = Var String
  | Fun Binder Expr
  | App Expr Expr
  | Let Binder Expr Expr
  | Fix [Binder] [Fn] Expr
  | If Expr Expr Expr
  | Match Expr [Constant] [Expr]
  | Tuple [Expr]
  | Prim Primitive [Expr]
  | Anno Expr Type
  | Const Constant
  | Sequence Expr Expr
  | Hole
  | Handle Expr [(String,Binder,Binder,Expr)]
  | Resume Expr Expr
  deriving (Show)


