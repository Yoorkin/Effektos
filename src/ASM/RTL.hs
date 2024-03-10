{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ASM.RTL where

import Prettyprinter
import Util.Prettyprint

data Program = Program [Fn] [BasicBlock]

data Fn = Fn String [BasicBlock] deriving Show

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
  | Jeq Operand Operand Operand
  | Call String
  | NewBlock Label
  | Ret
  deriving (Show)

(<=>) :: Doc ann -> Doc ann -> Doc ann
(<=>) a b = a <+> pretty "<-" <+> b

instance Pretty Operand where
  pretty (Reg i) = pretty $ "r" ++ show i
  pretty RLK = pretty "rtl"
  pretty (Val i) = pretty $ "v" ++ show i
  pretty (Label n) = pretty n
  pretty (I64 i) = pretty (show i)
  pretty Unit = pretty "unit"

instance Pretty BinOp where
  pretty = pretty . show

instance Pretty UryOp where
  pretty = pretty . show

instance Pretty Inst where
  pretty (Binary a op b c) = space <+> (pretty a <=> pretty b <+> pretty op <+> pretty c)
  pretty (Unary a op b) = space <+> (pretty a <=> pretty op <+> pretty b)
  pretty (Move a b) = space <+> (pretty a <=> pretty b)
  pretty (Load a b c) = space <+> (pretty a <=> pretty b <> brackets (pretty c))
  pretty (Store a b c) = space <+> (pretty a <> brackets (pretty c) <=> pretty b)
  pretty (Goto a) = space <+> (pretty "goto" <+> pretty a)
  pretty (Jeq a b c) = space <+> (pretty "jeq" <+> pretty a <+> pretty b <+> pretty c)
  pretty (Call f) = space <+> (pretty "call" <+> pretty f)
  pretty (NewBlock n) = pretty n <> pretty ":"
  pretty Ret = space <+> pretty "ret"

instance Pretty BasicBlock where
  pretty (BasicBlock l insts) = pretty l <> pretty ":" <> line <> vsep (map pretty insts)


instance Pretty Fn where
  pretty (Fn l bs) = pretty "function" <+> pretty l <+> braces (
                       nest 2 (line <> vsep (map pretty bs)) <> line)

instance Pretty Program where
  pretty (Program fns bs) = vsep (map pretty fns) <> line
         <> pretty "function entry" <+> braces (nest 2 (line <> vsep (map pretty bs)) <> line)

instance Show Program where
  show = render . pretty


