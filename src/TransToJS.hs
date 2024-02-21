{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module TransToJS (transl) where

import qualified Constant
import Flat
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import qualified Primitive as P
import Text.RawString.QQ

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

transl :: Flat.Program -> String
transl (Program f fs) = renderDoc $ pretty runtime <> hardline <> sepMapBy hardline translFn (f : fs)

runtime :: String
runtime = [r|
  // effect handler
  function _notnull(x){ 
    if(x===undefined){ 
      console.log('undefined') 
    } else {
      return x
    } 
  } 
  const _hds = new Map(); 
  function _push(eff,h){ if(_hds.has(eff)){ _hds.get(eff).push(h) }else{ _hds.set(eff,[h]) } } 
  function _pop(eff){ _hds.get(eff).pop() } 
  function _raise(eff,k,x,resume){ 
    const h = _hds.get(eff).slice(-1)[0]; 
    h[0](h,k,x,resume) 
  } 
  // entry
  main()    
|]

translExtern :: String -> [Name] -> Doc ann
translExtern f xs =
  let f' = case f of
        x -> x
   in pretty f' <> parens (sepMapBy comma (pretty . show) xs)

translVal :: Value -> Doc ann
translVal =
  \case
    Unit -> pretty "0"
    (I32 i) -> pretty i
    (Var n) -> pretty n
    (Proj i n) -> pretty "_notnull" <> parens (pretty n <> brackets (pretty i))
    (Tuple ns) -> brackets (sepMapBy comma (\z -> pretty "_notnull" <> parens (pretty z)) ns)
    (PrimOp op ns) ->
      let op2 a b op = pretty a <+> pretty op <+> pretty b
       in case (op, ns) of
            (P.Extern f, args) -> translExtern f args
            (P.Add2, [a, b]) -> op2 a b "+"
            (P.GT, [a, b]) -> op2 a b ">"
            (P.EQ, [a, b]) -> op2 a b "==="
            (P.LT, [a, b]) -> op2 a b "<"
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
            <> braces
              ( nested
                  ( hardline
                      <> sepMapBy hardline aux (zip ix es)
                      <> ( case fallback of
                             Nothing -> mempty
                             Just d -> pretty "default:" <> nested (hardline <> translExpr d)
                         )
                  )
                  <> hardline
              )
    (Handle e hdls) ->
      let aux [] = translExpr e
          aux ((eff, h) : hdls') =
            pretty "_push"
              <> parens (dquotes (pretty eff) <> comma <> pretty h)
                <//> aux hdls'
                <//> pretty "_pop"
              <> parens (dquotes (pretty eff))
       in aux hdls
    (Raise eff args) -> pretty "_raise" <> parens (dquotes (pretty eff) <> comma <> sepMapBy comma pretty args)
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

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> hardline <> b
