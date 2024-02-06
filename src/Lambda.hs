{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lambda where
import Data.Data
import Control.Lens.Plated
import Data.Data.Lens (uniplate)
import Primitive
import Constant

type Fn = (String, Expr)


data Repr
  = TaggedRep Int
  deriving (Eq,Ord,Show,Read,Data)

data Constructor
  = DataCon Repr
  | ConstCon Constant
  deriving (Eq,Ord,Show,Read,Data)

type Binder = String

data Expr
  = Var String
  | Abs String Expr
  | Let String Expr Expr
  | App Expr Expr
  | Fix [String] [Fn] Expr
  | Const Constant
  | Tuple [Expr]
  | Select Int Expr
  | PrimOp Primitive [Expr]
  | Constr Repr Expr
  | Decon Repr Expr
  | Switch Expr [(Int,Expr)] (Maybe Expr)
  | Handle Expr [(String,Binder,Binder,Expr)]
  | Resume Expr Expr
  deriving (Eq,Ord,Show,Read,Data)

instance Plated Repr where
  plate = uniplate

instance Plated Constructor where
  plate = uniplate

instance Plated Expr where
  plate = uniplate


