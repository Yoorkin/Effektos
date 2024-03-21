module Typing.QualifiedNames where
import Typing.Symbol

boolName, intName, unitName, arrowName :: QualifiedName
boolName = qualified "Builtin.Bool"
intName = qualified "Builtin.Int"
unitName = qualified "Builtin.Unit"
arrowName = qualified "Builtin.Arrow"


tupleTypeConstrName :: Int -> QualifiedName
tupleTypeConstrName i = qualified $ "Builtin.Tuple" ++ show i
