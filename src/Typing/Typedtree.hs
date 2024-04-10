{-# LANGUAGE LambdaCase #-}

module Typing.Typedtree where

import Common.Name
import Data.List (nub, (\\))
import Data.Map (Map)
import Prettyprinter
import Syntax.Constant
import Syntax.Primitive
import Typing.QualifiedNames (arrowName)
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
  deriving (Show,Eq)

data Expr
  = Var Type Name
  | Lam Type Pattern Expr
  | App Type Expr Expr
  | Let Type Pattern Expr Expr
  | Lit Type Constant
  | Case Type Expr [Pattern] [Expr]
  | Prim Type Primitive
  | Con Type Constr
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
  pretty = go
    where
      go (TypeVar var) = pretty $ case var of
        Unsolved n -> show n ++ "`"
        Solved n -> show n
      go (TypeForall quants ty) =
        pretty "∀" <> vsep (map (pretty . show) quants) <> pretty " . " <> go ty
      go (TypeConstr n [a, b]) | n == arrowName = parens (go a <+> pretty "->" <+> go b)
      go (TypeConstr n []) = pretty $ show n
      go (TypeConstr n tys) = pretty (show n) <+> vsep (map withParens tys)
      withParens expr@(TypeConstr _ tys) | not (null tys) = parens (go expr)
      withParens expr = go expr

instance Show Type where
  show = render . pretty

data DatatypeInfo
  = DataTypeInfo
  { datatypeName :: Name,
    datatypeQuants :: [Name],
    datatypeConstrs :: [Name]
  }
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
  (Prim ty _) -> ty
  (Lit ty _) -> ty
  (Con ty _) -> ty

isUnsolved :: TyVar -> Bool
isUnsolved (Unsolved _) = True
isUnsolved (Solved _) = False
