module RTL where

import CompileEnv

data Program = Program [Fn] [BasicBlock] deriving Show

data Fn = Fn Label [BasicBlock] deriving Show

type Label = String

data BasicBlock 
  = BasicBlock Label [Inst]
  deriving (Show)


data Operand
  = Reg Int
  | RLK
  | Val Int
  | Label Label 
  | I64 Integer
  | Unit
  deriving (Show)

data BinOp = Add2 | Sub2 | EQ deriving Show

data UryOp = Add1 | Sub1 deriving Show

data Inst
  = Binary Operand BinOp Operand Operand
  | Unary Operand UryOp Operand
  | Move Operand Operand
  | Load Operand Operand Operand -- dst src ofs
  | Store Operand Operand Operand -- dst val ofs
  | Goto Operand
  | Jif Operand Operand Operand 
  | Call String 
  | Ret 
  deriving (Show)
  

