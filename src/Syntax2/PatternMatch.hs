{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Syntax2.PatternMatch where

import Data.List (nub)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceWith)
import qualified Lambda.Lambda as L
import Prettyprinter
import qualified Syntax.Constant as Constant
import Syntax2.AST
import Syntax2.DefinitionInfo
import Syntax.Primitive as Primitive
import Util.CompileEnv
import Util.Prettyprint

transl = undefined

-- (Fun pat expr) ->
-- (Let pat expr1 expr2) ->

