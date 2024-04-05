module Typing.QualifiedNames where
import Common.CompileEnv
import Common.Name

boolName, intName, unitName, arrowName :: Name
boolName = synName "Bool"
intName = synName "Int"
unitName = synName "Unit"
arrowName = synName "Arrow"


tupleTypeConstrName :: Int -> Name
tupleTypeConstrName i = synName $ "Tuple" ++ show i
