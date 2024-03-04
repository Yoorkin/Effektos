module RTL where

data Program
  = Program [Fn]

data Fn = Fn String [BasicBlock] 

data BasicBlock 
  = Entry [Inst] 
  | Labeled String [Inst]
  deriving Show

type Reg = String

data BinOp = Add2 | Sub2 | EQ deriving Show

data UryOp = Add1 | Sub1 deriving Show

data Inst
  = Binary Reg BinOp Reg Reg
  | Unary Reg UryOp Reg
  | Move Reg Reg
  | Goto Reg
  | JEQ Reg Reg Reg 
  deriving (Show)
  

