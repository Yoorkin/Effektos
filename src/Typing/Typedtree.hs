{-# LANGUAGE LambdaCase #-}

module Typing.Typedtree where

import Data.List (nub, (\\))
import Data.Map (Map)
import Prettyprinter
import Syntax.Constant
import Syntax.Primitive
import Typing.QualifiedNames (arrowName)
import Common.Name
import Util.Prettyprint (render)

newtype Program
  = Program [TopBinding]
  deriving (Show)

data TopBinding = TopBinding Name Expr deriving (Show)

type Constr = Name

type Binder = Name

type Fn = (Pattern, Expr)

data Pattern
  = PatVar Type Name
  | PatCon Type Constr [Pattern]
  | PatLit Type Constant
  | PatOr Type Pattern Pattern
  | PatWildCard Type
  deriving (Show)

data Expr
  = Var Type Name
  | Lam Type Pattern Expr
  | App Type Expr Expr
  | Let Type Pattern Expr Expr
  | Lit Type Constant
  | Case Type Expr [Pattern] [Expr]
  | Prim Type Primitive [Expr]
  | Con Type Constr [Expr]
  deriving (Show)

type Arity = Int

data TyVar
  = Unsolved Name
  | Solved Name
  deriving (Show, Eq, Ord)

data Type
  = TypeVar TyVar
  | TypeConstr Name [Type]
  | TypeForall [TyVar] Type
  deriving (Eq, Ord)

instance Pretty Type where
  pretty = parens . go
    where
      go (TypeVar var) = pretty $ case var of
        Unsolved n -> show n ++ "`"
        Solved n -> show n
      go (TypeForall quants ty) =
        pretty "∀" <> vsep (map (pretty . show) quants) <> pretty " . " <> go ty
      go (TypeConstr n [a, b]) | n == arrowName = go a <+> pretty "->" <+> go b
      go (TypeConstr n []) = pretty $ show n
      go (TypeConstr n tys) = parens (pretty (show n) <+> vsep (map pretty tys))

instance Show Type where
  show = render . pretty

data DatatypeInfo
  = DataTypeInfo Name [Name] [Name]
  deriving (Show)

data TypeInfo
  = TypeConstrInfo Arity
  | QuantInfo Type
  deriving (Show)

data ValueInfo
  = ValueInfo Type
  | ConstrInfo Arity Type
  | LocalInfo Type
  deriving (Show)

data Table
  = Table
  { typeMap :: Map Name TypeInfo,
    valueMap :: Map Name ValueInfo,
    datatypeMap :: Map Name DatatypeInfo
  }

free :: Type -> [TyVar]
free ty = nub (go ty)
  where
    go (TypeVar var) = [var]
    go (TypeConstr _ xs) = concatMap go xs
    go (TypeForall bounded ty') = go ty' \\ bounded

typeOfPat :: Pattern -> Type
typeOfPat = \case
  (PatVar ty _) -> ty
  (PatCon ty _ _) -> ty
  (PatLit ty _) -> ty
  (PatOr ty _ _) -> ty
  (PatWildCard ty) -> ty

typeOfExpr :: Expr -> Type
typeOfExpr = \case
  (Var ty _) -> ty
  (Lam ty _ _) -> ty
  (App ty _ _) -> ty
  (Let ty _ _ _) -> ty
  (Case ty _ _ _) -> ty
  (Prim ty _ _) -> ty
  (Lit ty _) -> ty
  (Con ty _ _) -> ty

isUnsolved :: TyVar -> Bool
isUnsolved (Unsolved _) = True
isUnsolved (Solved _) = False
