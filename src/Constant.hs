{-# LANGUAGE DeriveDataTypeable #-}
module Constant(Constant(..)) where
import Data.Data

data Constant
  = Integer Int
  | Boolean Bool
  | Unit
  deriving (Eq,Ord,Show,Read,Data)
