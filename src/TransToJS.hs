{-# LANGUAGE LambdaCase #-}

module TransToJS (transl) where

import Flat
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import qualified Primitive as P

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

transl :: Flat.Program -> String
transl (Program f fs) = renderDoc $ sepMapBy hardline translFn (f : fs)

translVal :: Value -> Doc ann
translVal =
  \case
    Unit -> pretty "0"
    (I32 i) -> pretty i
    (Var n) -> pretty n
    (Proj i n) -> pretty n <> brackets (pretty i)
    (Tuple ns) -> brackets (sepMapBy comma pretty ns)
    (PrimOp op ns) ->
      let op2 a b op = pretty a <+> pretty op <+> pretty b in
      case (op, ns) of
        (P.Add2, [a, b]) -> op2 a b "+"
        (P.GT, [a, b]) -> op2 a b ">"
        (P.EQ, [a, b]) -> op2 a b "==="
        x -> pretty $ show op

translBinding :: Binding -> Doc ann
translBinding (Binding n v) = pretty "const" <+> pretty n <+> pretty "=" <+> translVal v

translExpr :: Expr -> Doc ann
translExpr =
  \case
    (Apply f xs) -> pretty f <> parens (sepMapBy comma pretty xs)
    (Switch n ix es fallback) ->
      let aux (i, e) = pretty "case" <+> pretty i <> pretty ": " <> translExpr e <> pretty "; break"
       in pretty "switch"
            <+> parens (pretty "Number" <> parens (pretty n))
            <> braces (nested (hardline <> sepMapBy hardline aux (zip ix es)
            <> ( case fallback of
                   Nothing -> mempty
                   Just d -> pretty "default:" <> nested (hardline <> translExpr d)
               )) <> hardline)
    (PushHdl {}) -> error ""
    (PopHdl {}) -> error ""
    (Raise {}) -> error ""
    (Exit n) -> pretty "console.log" <> parens (pretty n)

translFn :: Fn -> Doc ann
translFn (Fn n xs bs e) =
  pretty "function"
    <+> pretty n
    <> parens (sepMapBy comma pretty xs)
    <> braces
      ( nested
          ( hardline
              <> sepMapBy hardline translBinding bs
              <> hardline
              <> translExpr e
          )
          <> hardline
      )

sepMapBy :: Doc ann -> (a -> Doc ann) -> [a] -> Doc ann
sepMapBy sep' f xs = concatWith (\a b -> a <> sep' <> b) (map f xs)

nested :: Doc ann -> Doc ann
nested = nest 2

(</>) :: Doc ann -> Doc ann -> Doc ann
(</>) a b = a <> line <> b

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> hardline <> b
