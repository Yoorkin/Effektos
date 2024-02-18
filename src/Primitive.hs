{-# LANGUAGE DeriveDataTypeable #-}
module Primitive(Primitive(..)) where
import Data.Data (Data)
import Control.Lens 
import Data.Data.Lens (uniplate)

data Primitive
  = Add1
  | Sub1
  | Add2
  | Sub2
  | Mul
  | Div
  | GT
  | GE
  | NE
  | EQ
  | LE
  | LT
  | Not
  | And
  | Xor
  | Extern String
  deriving (Eq,Ord,Show,Read,Data)

instance Plated Primitive where
  plate = uniplate
