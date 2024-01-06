{-# LANGUAGE DeriveDataTypeable #-}

module Lambda where
import Data.Data
import Control.Lens.Plated
import Data.Data.Lens (uniplate)

type Fn = (String, Expr)

data Constant
  = Integer Int
  deriving (Eq,Ord,Show,Read,Data)

data Primitive
  = Add2
  | Sub2
  | Abort
  deriving (Eq,Ord,Show,Read,Data)

data Repr
  = TaggedRep Int
  deriving (Eq,Ord,Show,Read,Data)

data Constructor
  = DataCon Repr
  | ConstCon Constant
  deriving (Eq,Ord,Show,Read,Data)

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
  | Constr Repr Expr
  | Decon Repr Expr
  | Switch Expr [(Int,Expr)] (Maybe Expr)
  deriving (Eq,Ord,Show,Read,Data)

instance Plated Primitive where
  plate = uniplate

instance Plated Repr where
  plate = uniplate

instance Plated Constructor where
  plate = uniplate

instance Plated Expr where
  plate = uniplate


