{-# LANGUAGE LambdaCase #-}
module Typing.Typedtree where

import Syntax.Constant
import Syntax.Primitive
import Typing.Symbol
import Util.CompileEnv
import Data.Map (Map)
import Data.List (nub, (\\))

newtype Program 
  = Program [Decl] 
  deriving Show

data Decl = TopBinding Name Expr deriving Show

type Constr = Name

type Binder = Name

type Fn = (Pattern, Expr)

data Pattern
  = PatVar Type Name
  | PatConstr Type Constr [Pattern]
  | PatConstant Type Constant
  | PatTuple Type [Pattern]
  | PatOr Type Pattern Pattern
  | PatWildCard Type
  deriving (Show)

data Expr
  = Var Type Name
  | Fun Type Pattern Expr
  | App Type Expr Expr
  | Let Type Pattern Expr Expr
  | Fix Type [Binder] [Fn] Expr
  | If Type Expr Expr Expr
  | Match Type Expr [Pattern] [Expr]
  | Tuple Type [Expr]
  | Prim Type Primitive [Expr]
  | Const Type Constant
  | Seq Type Expr Expr
  | Hole Type
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
  deriving (Show, Eq, Ord)


data DatatypeInfo
  = DataTypeInfo Name [Name] [Name]
  deriving (Show)

data TypeInfo
  = TypeConstrInfo Name Arity

data ValueInfo
  = ValueInfo Name Type
  | ConstrInfo Name Arity Type
  | DummyInfo 

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
      (PatConstant ty _) -> ty

typeOfExpr :: Expr -> Type
typeOfExpr = \case 
  (Var ty _) -> ty
  (Fun ty _ _) -> ty
  (App ty _ _ ) -> ty
  (Let ty _ _ _) -> ty
  (Fix ty _ _ _) -> ty
  (If ty _ _ _) -> ty
  (Match ty _ _ _ ) -> ty
  (Tuple ty _) -> ty
  (Prim ty _ _) -> ty
  (Const ty _) -> ty
  (Seq ty _ _) -> ty
  (Hole ty) -> ty

isUnsolved :: TyVar -> Bool
isUnsolved (Unsolved _) = True
isUnsolved (Solved _) = False
