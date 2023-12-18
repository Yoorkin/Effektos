{-# LANGUAGE DeriveDataTypeable #-}

module Lambda where
import Data.Data
import Data.Generics.Uniplate.Data

type Fn = (String, Expr)

data Constant
  = Integer Int
  deriving (Show, Data, Typeable)

data Primitive
  = Add2
  | Sub2
  | Abort
  deriving (Show, Data, Typeable)

data Represent
  = TaggedRep Int
  deriving (Show, Data, Typeable)

data Constructor
  = DataCon Represent
  | ConstCon Constant
  deriving (Show, Data, Typeable)

data Expr
  = Var String
  | Abs String Expr
  | Let String Expr Expr
  | App Expr Expr
  | Fix [(String, Fn)] Expr
  | Const Constant
  | Tuple [Expr]
  | Select Int Expr
  | PrimOp Primitive [Expr]
  | Constr Represent Expr
  | Decon Represent Expr
  | Switch Expr [Int] (Maybe Expr)
  deriving (Show, Data, Typeable)
