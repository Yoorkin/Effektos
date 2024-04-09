{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typing.Builtin where

import Syntax.Constant as Constant
import Syntax.Primitive as Op
import Typing.QualifiedNames
import Typing.Typedtree
import Common.Name

boolType, intType, unitType :: Type
boolType = TypeConstr boolName []
intType = TypeConstr intName []
unitType = TypeConstr unitName []

arrowType :: Type -> Type -> Type
arrowType a b = TypeConstr arrowName [a, b]

makeApps :: Type -> Expr -> [Expr] -> Expr
makeApps _ _ [] = error "invalid args"
makeApps retTy f [x] = App retTy f x
makeApps retTy f (x:xs) = makeApps retTy (App ty f x) xs
  where ty = case typeOfExpr f of
               (TypeConstr n [_,b]) | n == arrowName -> b
               _ -> error $ "makeApps: type of " ++ show x ++ " is not a arrow"

-- Type of tuple, consis of it's elements.
tupleType :: [Type] -> Type
tupleType tys = TypeConstr (tupleTypeCon (length tys)) tys

-- Type of tuple constructor.
-- It accepts curried N type argument and return a N-tuple type.
tupleConType :: [Type] -> Type
tupleConType argTys = go argTys
  where 
      go [] = tupleType argTys
      go (x:xs) = x |-> go xs

-- Tuple type constructor and tuple constructor.
-- Their names are ("Tuple" ++ show arity).
tupleTypeCon, tupleCon :: Int -> Name
tupleTypeCon i = synName $ "Tuple" ++ show i
tupleCon = tupleTypeCon

builtinTypes :: [(Name, TypeInfo)]
builtinTypes =
  [ (boolName, TypeConstrInfo 0),
    (intName, TypeConstrInfo 0),
    (unitName, TypeConstrInfo 0),
    (arrowName, TypeConstrInfo 2)
  ]

infixr 5 |->

(|->) :: Type -> Type -> Type
a |-> b = arrowType a b

splitArrowType :: Type -> ([Type], Type)
splitArrowType (TypeForall _ ty) = splitArrowType ty
splitArrowType ty = aux ty []
  where
    aux (TypeConstr name [a, b]) acc | name == arrowName = aux b (a : acc)
    aux _ (ret : acc) = (reverse acc, ret)
    aux a b = error $ show (a,b)

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
