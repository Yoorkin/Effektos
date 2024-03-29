{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lambda.Lambda where

import Control.Lens.Plated
import Data.Data
import Data.Data.Lens (uniplate)
import Prettyprinter
import Syntax.Constant
import Syntax.Primitive
import Util.CompileEnv
import Util.Prettyprint

type Fn = (Name, Expr)

data Repr
  = TaggedRep Int
  deriving (Eq, Ord, Show, Read, Data)

data Constructor
  = DataCon Repr
  | ConstCon Constant
  deriving (Eq, Ord, Show, Read, Data)

--
-- Currently the memory layout of datatype is simple:
--  +-----+---------------------------+
--  | tag | playload1, ..., playloadN |
--  +-----+---------------------------+
-- Each tag and playload is occupying one word, the length of playload are determined by tag.
-- Some specific optimization can be used in future.
--

type Effect = String

data Expr
  = Var Name
  | Abs Name Expr
  | Let Name Expr Expr
  | App Expr Expr
  | Fix [Name] [Fn] Expr
  | Const Constant
  | Tuple [Expr]
  | Select Int Expr
  | PrimOp Primitive [Expr]
  | Constr Repr Expr
  | Switch Expr [(Constant, Expr)] (Maybe Expr)
  | Handle Expr [(Effect, Name, Name, Expr)]
  | Raise Effect Expr
  | Resume Expr Expr
  deriving (Eq, Ord, Read, Data)

isValue :: Expr -> Bool
isValue = \case
  (Var _) -> True
  (Abs {}) -> True
  (Const {}) -> True
  (Tuple {}) -> True
  (Select {}) -> True
  (PrimOp {}) -> True
  (Constr {}) -> True
  _ -> False

instance Plated Repr where
  plate = uniplate

instance Plated Constructor where
  plate = uniplate

instance Plated Expr where
  plate = uniplate

instance Pretty Expr where
  pretty (Var n) = pretty n
  pretty (Abs n e) = group (pretty "fun" <+> pretty n <+> pretty "->" <> nest 2 (line <> pretty e))
  pretty (Let n e1 e2) = group (pretty "let" <+> group (pretty n <+> pretty "=" <+> pretty e1 <> line <> pretty "in") <> line <> pretty e2)
  pretty (App a b) = parens (pretty a <+> pretty b)
  pretty (Fix ns fns e) = group (pretty "let rec" <+> nest 2 (vcat (zipWith f ns fns)) <> line <> pretty "in" <> line <> pretty e)
    where
      f n (arg, expr) = pretty n <+> pretty "=" <+> pretty "fun" <+> pretty (show arg) <+> pretty "->" <+> group (nest 2 (line <> pretty expr))
  pretty (Const c) = pretty (show c)
  pretty (Tuple xs) = parens (concatWith (\a b -> a <> pretty "," <+> b) (map pretty xs))
  pretty (Select i e) = pretty e <> pretty "[" <> pretty i <> pretty "]"
  pretty (PrimOp op es) = pretty (show op) <+> fillSep (map pretty es)
  pretty (Constr repr expr) = pretty "Constr" <+> pretty (show repr) <+> pretty expr
  pretty (Switch expr bs fb) = group (align (pretty "case" <+> pretty expr <+> pretty "of" <> (line <> vcat (map f bs ++ [g fb]))))
    where
      f (c, e) = group $ pretty "|" <+> pretty (show c) <+> pretty "->" <> nest 4 (line <> pretty e)
      g (Just e) = group $ pretty "|" <+> pretty "_" <+> pretty "->" <> nest 4 (line <> pretty e)
      g Nothing = mempty
  pretty (Handle e hdls) = pretty "handle" <+> pretty e <+> pretty "with" <> line <> hdls'
    where
      hdls' = hsep (map f hdls)
      f :: (Effect, Name, Name, Expr) -> Doc ann
      f (eff, k, x, e') = pretty "|" <+> parens (pretty eff <+> pretty k <+> pretty x) <+> pretty "->" <+> nest 2 (pretty e')
  pretty (Raise eff e) = pretty "raise" <+> pretty eff <+> pretty e

instance Show Expr where
  show = render . pretty
