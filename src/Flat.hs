module Flat where

import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Primitive

type Name = String

data Program
  = Program Fn [Fn]

data Fn
  = Fn Name [Name] [Binding] Expr
  deriving (Show)

data Binding = Binding Name Value
  deriving (Show)

data Value
  = Unit
  | I32 Int
  | Var Name
  | Proj Int Name
  | Tuple [Name]
  | PrimOp Primitive [Name]
  deriving (Show)

instance Pretty Value where
  pretty Unit = pretty "Unit"
  pretty (I32 v) = pretty v
  pretty (Var n) = pretty n
  pretty (Proj i n) = pretty n <> brackets (pretty i)
  pretty (Tuple ns) = parens (sepMapBy (comma <> space) pretty ns)
  pretty v@(PrimOp {}) = pretty $ show v

data Expr
  = Apply Name [Name]
  | Switch Name [Int] [Expr] (Maybe Expr)
  | Exit Name
  deriving (Show)

(</>) a b = a <> line <> b

(<//>) a b = a <> hardline <> b

nested = nest 4

sepMapBy sep f xs = concatWith (\a b -> a <> sep <> b) (map f xs)

instance Pretty Program where
  pretty (Program m fns) = concatWith (<//>) (map pretty (m : fns))

instance Pretty Binding where
  pretty (Binding n v) = pretty n <+> pretty "=" <+> pretty v

instance Pretty Fn where
  pretty (Fn n args bindings expr) =
    pretty n
      <> parens (sepMapBy (comma <> space) pretty args)
      <> pretty ":"
      <> nested (hardline <> concatWith (<>) (map (\x -> pretty x <> hardline) bindings) <> pretty expr)
      <> hardline

instance Pretty Expr where
  pretty (Apply f args) = pretty f <> parens (sepMapBy (comma <> space) pretty args)
  pretty (Switch cond index branches fallback) =
    pretty "switch"
      <+> pretty cond
      <> colon
      <> nested (hardline <> sepMapBy hardline f (zip index branches))
      <> case fallback of (Just f) -> pretty "_ ->" <+> pretty f; Nothing -> pretty ""
    where
      f (i, e) = pretty (i :: Int) <+> pretty "->" <+> pretty e
  pretty (Exit n) = pretty "exit" <+> pretty n

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show Program where
  show = renderDoc . pretty
