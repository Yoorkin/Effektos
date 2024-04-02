{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax2.DefinitionInfo where

import Syntax2.AST

-- The translation of pattern matching needs some information:
--  1. The arity of each data constructor.
--  2. The data type to which each data constructor belongs.
--  3. All constructors of a given data type.
--
-- additionally, to distinguish between `PatVar id` and `PatConstr id []` for `id`:
--  1. Provide all constructors name
--

type Arity = Int

type DataType = String

data ConstrInfo = ConstrInfo Arity DataType deriving (Ord, Eq, Show)

data DataTypeInfo = DataTypeInfo [Constr] deriving (Ord, Eq, Show)

data EffectInfo = EffectInfo Arity deriving (Ord,Eq,Show)
