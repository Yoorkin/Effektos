{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module CPS where

import CompileEnv
import qualified Constant as C
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Primitive

data Value
  = Var Name
  | I32 Int
  | Unit
  | Tuple [Name]
  | Cont (Maybe Name) Name Term -- env x e
  | Fn Name (Maybe Name) [Name] Term -- k env x e
  deriving (Eq, Show, Ord, Read, Data)

type Effect = String

type Cont = Name

type Function = Name

type Argument = Name

type Closure = Name

data Term
  = LetVal Name Value Term
  | LetSel Name Int Name Term -- TODO: Value to String, the Value should be (Var x)
  | LetCont Name (Maybe Name) Name Term Term
  | LetFns [(Name, Value)] Term
  | Continue Name (Maybe Name) Name
  | Apply Function Cont (Maybe Closure) [Argument]
  | LetPrim Name Primitive [Name] Term
  | Switch Name [C.Constant] [Term]
  | Handle Term [(Effect, Function)]
  | Raise Effect Cont [Argument] -- h k x
  | Halt Name
  deriving (Eq, Show, Ord, Read, Data)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate
