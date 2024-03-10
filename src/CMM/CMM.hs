{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module CMM.CMM where

import Util.CompileEnv (Name)
import Prettyprinter
import Prettyprinter.Render.String
import Syntax.Primitive as Primitive

data Program = Program [Fn] Expr [Block]

data Fn
  = Fn Name [Name] Expr [Block]
  deriving (Show)

type Label = Name

type Block = (Label, Expr)

type Var = Name

data Expr
  = Define Var Expr
  | Malloc Var Int Expr
  | Select Var Int Var Expr
  | Mutate Var Int Var Expr
  | Assign Var Value Expr
  | PrimOp Var Primitive [Var] Expr
  | Switch Var [(Value, Expr)] (Maybe Expr)
  | Jump Label
  | Call Var Var [Var] Expr
  | Return Var
  deriving (Show)

data Value
  = Integer Int
  | String String
  | Unit
  | Variable Var
  deriving (Show)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show Program where
  show = renderDoc . pretty

instance Pretty Program where
  pretty (Program fns e blks) =
    vsep (map pretty fns)
      <> line
      <> pretty "int" <+> pretty "main"
      <> parens mempty
      <> braces (nest 2 (line <> pretty e) <> line <> vsep (map blk2doc blks) <> line)

blk2doc :: (Pretty a1, Pretty a2) => (a1, a2) -> Doc ann
blk2doc (label, expr) = pretty label <> pretty ":" <> nest 2 (hardline <> pretty expr)

instance Pretty Fn where
  pretty (Fn n args e blks) =
    wordDoc <+> pretty n
      <> parens (concatWith (\a b -> a <> comma <+> b) $ map (\a -> wordDoc <+> pretty a) args)
      <> braces (nest 2 (line <> pretty e) <> line <> vsep (map blk2doc blks) <> hardline)

instance Pretty Value where
  pretty (Integer i) = pretty i
  pretty (String s) = pretty "\"" <> pretty s <> pretty "\""
  pretty Unit = pretty "0"
  pretty (Variable n) = parens wordDoc <> pretty (show n)

sepBy :: Doc a -> [Doc a] -> Doc a
sepBy s xs = case xs of
  [] -> mempty
  _ -> concatWith (\x acc -> x <> s <> acc) xs

wordDoc :: Doc ann
wordDoc = pretty "uint64_t"

instance Pretty Expr where
  pretty (Define n e) = wordDoc <+> pretty n <> semi <> line <> pretty e
  pretty (Malloc n i e) =
    pretty n <+> pretty "=" <+> parens wordDoc
      <> pretty "malloc"
      <> parens (pretty i <+> pretty "*" <+> pretty "sizeof" <> parens wordDoc)
      <> semi
      <> line
      <> pretty e
  pretty (Select n1 i n2 e) =
    pretty n1 <+> pretty "=" <+> parens (parens (wordDoc <> pretty "*") <> pretty n2)
      <> brackets (pretty i)
      <> semi
      <> line
      <> pretty e
  pretty (Mutate n1 i n2 e) =
    parens (parens (wordDoc <> pretty "*") <> pretty n1)
      <> brackets (pretty i)
        <+> pretty "="
        <+> parens wordDoc <> pretty n2
      <> semi
      <> line
      <> pretty e
  pretty (Assign n v e) =
    pretty n <+> pretty "=" <+> pretty v <> semi <> line <> pretty e
  pretty (PrimOp n op args e) =
    let doc =
          case (op, args) of
            (Primitive.EQ, [a, b]) -> pretty a <+> pretty "==" <+> pretty b
            _ -> pretty (show (op, args))
     in pretty n
          <+> pretty "="
          <+> parens doc
          <> semi
          <> line
          <> pretty e
  pretty (Switch c cases fallback) =
    pretty "switch" <+> parens (pretty c)
      <> braces (nest 2
        ( line <> vsep (map g cases ++ f fallback)) <> line
        )
    where
      g (v, e) =
        pretty "case" <+> pretty v
          <> pretty ":"
          <+> braces (nest 2 (line <> pretty e) <> line)
      f fb = case fb of
        Nothing -> []
        Just e -> [pretty "default" <> pretty ":" <+> braces (nest 2 (pretty e))]
  pretty (Jump label) = pretty "goto" <+> pretty label <> semi
  pretty (Call n1 f args e) =
    pretty n1 <+> pretty "=" 
      <+> parens (parens (wordDoc <> parens (pretty "*") <> parens (sepBy comma (map (const wordDoc) args))) <> pretty f) <+> parens (sepBy (comma <> space) (map pretty args))
      <> semi
      <> line
      <> pretty e
  pretty (Return n) = pretty "return" <+> pretty n <> semi
