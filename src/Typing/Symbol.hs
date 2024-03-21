{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE EmptyCase #-}

module Typing.Symbol where

import Data.List (nub, (\\))
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
  = -- name, arity
    TypeConstrInfo QualifiedName Arity
  | -- name, arity, scheme type
    TypeSchemeInfo QualifiedName Arity Type
  | -- name, data constructor names
    DataTypeInfo QualifiedName [QualifiedName]
  | -- name, arity, data type name
    DataConstrInfo QualifiedName Arity [Type] QualifiedName
  | -- name, effect constructor names
    EffectTypeInfo QualifiedName [QualifiedName]
  | -- name, arity, effect type name
    EffectConstrInfo QualifiedName Arity [Type] QualifiedName
  deriving (Show, Eq, Ord)

type Table = Map.Map QualifiedName Symbol

data SymbolTable =
    SymbolTable Table Table Table
  deriving Show

makeSymbolTable :: [Symbol] -> SymbolTable
makeSymbolTable syms = SymbolTable types constrs values
     where
       zipWithName = Map.fromList . map (\x -> (qualifiedName x, x))
       types = zipWithName $ filter isTypeInfo syms
       constrs = zipWithName $ filter isConstrInfo syms
       values = Map.empty
       isTypeInfo = \case
                      (DataTypeInfo {}) -> True
                      (EffectTypeInfo {}) -> True
                      (TypeConstrInfo {}) -> True
                      _ -> False
       isConstrInfo = \case
                       (DataConstrInfo {}) -> True
                       (EffectConstrInfo {}) -> True



lookupType, lookupConstr :: QualifiedName -> SymbolTable -> Maybe Symbol
lookupType k (SymbolTable t _ _) = Map.lookup k t
lookupConstr k (SymbolTable _ c _) = Map.lookup k c

qualifiedName :: Symbol -> QualifiedName
qualifiedName = \case
  (TypeConstrInfo n _) -> n
  (TypeSchemeInfo n _ _) -> n
  (DataTypeInfo n _) -> n
  (DataConstrInfo n _ _ _) -> n
  (EffectTypeInfo n _) -> n
  (EffectConstrInfo n _ _ _) -> n

free :: Type -> [TyVar]
free ty = nub (go ty)
  where
    go (TypeVar var) = [var]
    go (TypeSolved _) = []
    go (TypeConstr _ xs) = concatMap go xs
    go (TypeForall bounded ty') = go ty' \\ bounded
