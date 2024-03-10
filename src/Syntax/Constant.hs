{-# LANGUAGE DeriveDataTypeable #-}
module Syntax.Constant(Constant(..)) where
import Data.Data

data Constant
  = Integer Int
  | Boolean Bool
  | String String
  | Unit
  deriving (Eq,Ord,Show,Read,Data)
