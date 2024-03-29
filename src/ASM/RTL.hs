{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ASM.RTL where

import Prettyprinter
import Util.Prettyprint

data Program = Program [Fn] [Inst]

data Fn = Fn String [Inst] deriving Show

type Label = String

data Operand
  = Reg Int
  | RLK
  | Arg Int
  | Val Int
  | Label Label
  | Foreign String
  | I64 Integer
  | Unit
  deriving (Eq,Ord)

data BinOp = Add2 | Sub2 | EQ deriving Show

data UryOp = Add1 | Sub1 deriving Show

type Arity = Int

data Inst
  = Binary Operand BinOp Operand Operand
  | Unary Operand UryOp Operand
  | Move Operand Operand
  | Load Operand Operand Operand -- dst src ofs
  | Store Operand Operand Operand -- dst val ofs
  | Goto Operand
  | Jeq Operand Operand Operand
  | Call Operand Arity
  | NewBlock Label
  | Ret

(<=>) :: Doc ann -> Doc ann -> Doc ann
(<=>) a b = a <+> pretty "<-" <+> b

instance Show Operand where
  show = render . pretty

instance Pretty Operand where
  pretty (Reg i) = pretty $ "r" ++ show i
  pretty RLK = pretty "rlk"
  pretty (Val i) = pretty $ "v" ++ show i
  pretty (Label n) = pretty n
  pretty (Foreign n) = pretty n
  pretty (I64 i) = pretty (show i)
  pretty Unit = pretty "unit"

instance Pretty BinOp where
  pretty = pretty . show

instance Pretty UryOp where
  pretty = pretty . show

instance Show Inst where
  show = render . pretty

instance Pretty Inst where
  pretty (Binary a op b c) = space <+> (pretty a <=> pretty b <+> pretty op <+> pretty c)
  pretty (Unary a op b) = space <+> (pretty a <=> pretty op <+> pretty b)
  pretty (Move a b) = space <+> (pretty a <=> pretty b)
  pretty (Load a b c) = space <+> (pretty a <=> pretty b <> brackets (pretty c))
  pretty (Store a b c) = space <+> (pretty a <> brackets (pretty c) <=> pretty b)
  pretty (Goto a) = space <+> (pretty "goto" <+> pretty a)
  pretty (Jeq a b c) = space <+> (pretty "jeq" <+> pretty a <+> pretty b <+> pretty c)
  pretty (Call f a) = space <+> (pretty "call" <+> pretty f <+> pretty a)
  pretty (NewBlock n) = pretty n <> pretty ":"
  pretty Ret = space <+> pretty "ret"

instance Pretty Fn where
  pretty (Fn l bs) = pretty "function" <+> pretty l <+> braces (
                       nest 2 (line <> vsep (map pretty bs)) <> line)

instance Pretty Program where
  pretty (Program fns bs) = vsep (map pretty fns) <> line
         <> pretty "function entry" <+> braces (nest 2 (line <> vsep (map pretty bs)) <> line)

instance Show Program where
  show = render . pretty


