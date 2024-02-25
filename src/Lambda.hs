{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lambda where

import CompileEnv
import Constant
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens (uniplate)
import Primitive

type Fn = (Name, Expr)

data Repr
  = TaggedRep Int
  deriving (Eq, Ord, Show, Read, Data)

data Constructor
  = DataCon Repr
  | ConstCon Constant
  deriving (Eq, Ord, Show, Read, Data)

--
-- Currently the memory layout of datatype is simple:
--  +-----+---------------------------+
--  | tag | playload1, ..., playloadN |
--  +-----+---------------------------+
-- Each tag and playload is occupying one word, the length of playload are determined by tag.
-- Some specific optimization can be used in future.
-- 

type Effect = String

data Expr
  = Var Name
  | Abs Name Expr
  | Let Name Expr Expr
  | App Expr Expr
  | Fix [Name] [Fn] Expr
  | Const Constant
  | Tuple [Expr]
  | Select Int Expr
  | PrimOp Primitive [Expr]
  | Constr Repr Expr
  | Switch Expr [(Int, Expr)] (Maybe Expr)
  | Handle Expr [(Effect, Name, Name, Expr)]
  | Raise Effect Expr
  | Resume Expr Expr
  deriving (Eq, Ord, Show, Read, Data)

instance Plated Repr where
  plate = uniplate

instance Plated Constructor where
  plate = uniplate

instance Plated Expr where
  plate = uniplate

