{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Syntax2.SyntaxToLambda (transProg) where

import Util.CompileEnv
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Lambda.Lambda as L
import Syntax.Constant as Constant
import qualified Syntax2.PatternMatch as PatternMatch
import Syntax2.AST as S
import Syntax2.DefinitionInfo
import Data.Bifunctor (Bifunctor(first))

transProg :: Program -> CompEnv L.Expr
transProg (Program defs expr) = undefined
 
preprocessDefs :: [Definition] -> (Map String DataTypeInfo, Map String ConstrInfo, Map String EffectInfo)
preprocessDefs = undefined

preprocessPattern :: Map String ConstrInfo -> Pattern -> Pattern
preprocessPattern = undefined


translExpr = undefined
