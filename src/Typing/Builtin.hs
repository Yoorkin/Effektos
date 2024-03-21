{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typing.Builtin where

import Syntax.Constant as Constant
import Syntax.Primitive as Op
import Typing.QualifiedNames
import Typing.Symbol

boolType, intType, unitType :: Type
boolType = TypeConstr boolName []
intType = TypeConstr intName []
unitType = TypeConstr unitName []

arrowType :: Type -> Type -> Type
arrowType a b = TypeConstr arrowName [a, b]

tupleType :: [Type] -> Type
tupleType tys = TypeConstr (tupleTypeConstrName (length tys)) tys

builtinSymbols :: [(QualifiedName, Symbol)]
builtinSymbols =
  [ (boolName, TypeConstrInfo boolName 0),
    (intName, TypeConstrInfo intName 0),
    (unitName, TypeConstrInfo unitName 0),
    (arrowName, TypeConstrInfo arrowName 2)
  ]

infixr 5 |->

(|->) :: Type -> Type -> Type
a |-> b = arrowType a b

splitArrowType :: Type -> ([Type], Type)
splitArrowType ty = aux ty []
  where
    aux (TypeConstr name [a, b]) acc | name == arrowName = aux b (a : acc)
    aux _ (ret : acc) = (reverse acc, ret)

typeOfConstant :: Constant -> Type
typeOfConstant = \case
  (Constant.Integer _) -> intType
  (Constant.Boolean _) -> boolType
  Constant.Unit -> unitType
  _ -> error ""

typeOfPrim :: Primitive -> Type
typeOfPrim op =
  case op of
    Op.Add1 -> intType |-> intType
    Op.Sub1 -> intType |-> intType
    Op.Add2 -> intType |-> intType |-> intType
    Op.Sub2 -> intType |-> intType |-> intType
    Op.Mul -> intType |-> intType |-> intType
    Op.Div -> intType |-> intType |-> intType
    Op.GT -> intType |-> intType |-> boolType
    Op.GE -> intType |-> intType |-> boolType
    Op.NE -> intType |-> intType |-> boolType
    Op.EQ -> intType |-> intType |-> boolType
    Op.LE -> intType |-> intType |-> boolType
    Op.LT -> intType |-> intType |-> boolType
    Op.Not -> boolType |-> boolType
    Op.And -> boolType |-> boolType |-> boolType
    Op.Xor -> boolType |-> boolType |-> boolType
    Op.Or -> boolType |-> boolType |-> boolType
