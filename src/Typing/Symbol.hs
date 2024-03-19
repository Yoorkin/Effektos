module Typing.Symbol where

import Syntax.Primitive as Op
import Data.List ((\\), nub)

type Arity = Int

type TyVar = String

data Type
  = TypeVar TyVar
  | TypeConstr Symbol [Type]
  | TypeForall [TyVar] Type
  deriving (Show,Eq)

data Symbol
  = TypeConstructor String Arity
  | TypeScheme String Arity Type
  deriving (Show,Eq)

free :: Type -> [TyVar]
free ty = nub (go ty)
    where
        go (TypeVar var) = [var]
        go (TypeConstr _ xs) = concatMap go xs
        go (TypeForall bounded ty') = go ty' \\ bounded 

arrowTyConstr :: Symbol
arrowTyConstr = TypeConstructor "arrow" 2

zeroTyConstr s = TypeConstr (TypeConstructor s 0) []

boolType,intType,unitType :: Type
boolType = zeroTyConstr "bool"
intType = zeroTyConstr "int"
unitType = zeroTyConstr "unit"

makeArrowType :: Type -> Type -> Type
makeArrowType a b = TypeConstr arrowTyConstr [a, b]

makeTupleType :: [Type] -> Type
makeTupleType elems = TypeConstr tupleSymbol elems
  where
    arity = length elems
    tupleSymbol = TypeConstructor ("tuple" ++ show arity) arity

splitArrowType :: Type -> ([Type], Type)
splitArrowType ty = aux ty []
  where
    aux (TypeConstr (TypeConstructor "arrow" 2) [a, b]) acc = aux b (a : acc)
    aux _ (ret:acc) = (reverse acc, ret)

infixr 5 |->
(|->) :: Type -> Type -> Type
a |-> b = makeArrowType a b

typeOfPrim :: Primitive -> Type
typeOfPrim op = 
 case op of
  Op.Add1 -> intType |-> intType
  Op.Sub1 -> intType |-> intType 
  Op.Add2-> (intType |-> intType |-> intType)
  Op.Sub2-> (intType |-> intType |-> intType)
  Op.Mul -> (intType |-> intType |-> intType)
  Op.Div -> (intType |-> intType |-> intType)
  Op.GT -> (intType |-> intType |-> boolType)
  Op.GE -> (intType |-> intType |-> boolType)
  Op.NE -> (intType |-> intType |-> boolType)
  Op.EQ -> (intType |-> intType |-> boolType)
  Op.LE -> (intType |-> intType |-> boolType)
  Op.LT -> (intType |-> intType |-> boolType)
  Op.Not -> boolType |-> boolType
  Op.And -> boolType |-> boolType |-> boolType
  Op.Xor -> boolType |-> boolType |-> boolType
  Op.Or -> boolType |-> boolType |-> boolType
  

