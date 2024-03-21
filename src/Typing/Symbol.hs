{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typing.Symbol where

import Data.List (nub, (\\))
import Data.List.Split (splitOn)

type Arity = Int

type TyVar = String

data Type
  = TypeVar TyVar
  | TypeSolved QualifiedName
  | TypeConstr QualifiedName [Type]
  | TypeForall [TyVar] Type
  deriving (Show, Eq, Ord)

data QualifiedName
  = TermName String
  | QualifiedPath String QualifiedName
  deriving (Show, Eq, Ord)

qualified :: String -> QualifiedName
qualified = f . splitOn "."
  where
    f [x] = TermName x
    f (x : xs) = QualifiedPath x (f xs)

data Symbol
  = TypeConstrInfo QualifiedName Arity
  | TypeSchemeInfo QualifiedName Arity Type
  | -- data constructors
    DataTypeInfo QualifiedName [QualifiedName]
  | DataConstrInfo QualifiedName Arity Type
  | -- effect constructors
    EffectTypeInfo [QualifiedName]
  | EffectConstrInfo Arity Type
  deriving (Show, Eq, Ord)

free :: Type -> [TyVar]
free ty = nub (go ty)
  where
    go (TypeVar var) = [var]
    go (TypeSolved _) = []
    go (TypeConstr _ xs) = concatMap go xs
    go (TypeForall bounded ty') = go ty' \\ bounded
