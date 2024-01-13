module Flat where

import qualified Lambda as L

data Name = String

data Program
  = Program Fn [Fn]

data Fn
  = Fn Name [Name] [Binding] Expr

data Binding = Binding Name Value

data Value
  = Unit
  | I32 Int
  | Var Name
  | Proj Name
  | Tuple [Name]
  | PrimOp L.Primitive [Name]

data Expr
  = Apply Name [Name]
  | Switch Name [Expr]
