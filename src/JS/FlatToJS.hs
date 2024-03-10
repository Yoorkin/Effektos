{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module JS.FlatToJS (transl) where

import JS.Flat as Flat
import Prettyprinter
import qualified Syntax.Constant as C
import qualified Syntax.Primitive as P
import Text.RawString.QQ
import Util.Prettyprint

transl :: Flat.Program -> String
transl (Program f fs) = render $ pretty runtime <> hardline <> sepMap hardline translFn (f : fs)

runtime :: String
runtime =
  [r|
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
   in pretty f' <> parens (sepMap comma (pretty . show) xs)

translVal :: Value -> Doc ann
translVal =
  \case
    Unit -> pretty "0"
    (I32 i) -> pretty i
    (Var n) -> pretty n
    (Proj i n) -> pretty "_notnull" <> parens (pretty n <> brackets (pretty i))
    (Tuple ns) -> brackets (sepMap comma (\z -> pretty "_notnull" <> parens (pretty z)) ns)
    (PrimOp op ns) ->
      let op2 a b o = pretty a <+> pretty o <+> pretty b
       in case (op, ns) of
            (P.Extern f, args) -> translExtern f args
            (P.Add2, [a, b]) -> op2 a b "+"
            (P.GT, [a, b]) -> op2 a b ">"
            (P.EQ, [a, b]) -> op2 a b "==="
            (P.LT, [a, b]) -> op2 a b "<"
            _ -> pretty $ show op

translBinding :: Binding -> Doc ann
translBinding (Binding n v) = pretty "const" <+> pretty n <+> pretty "=" <+> translVal v

translConstant :: C.Constant -> Doc ann
translConstant = \case
  (C.Integer i) -> pretty (show i)
  (C.String s) -> parens (pretty $ show s)
  (C.Boolean b) -> pretty (if b then "true" else "false")
  C.Unit -> pretty "0"

translExpr :: Expr -> Doc ann
translExpr =
  \case
    (Apply f xs) -> pretty f <> parens (sepMap comma pretty xs)
    (Switch n ix es fallback) ->
      let aux (i, e) = pretty "case" <+> translConstant i <> pretty ": " <> translExpr e <> pretty "; break"
       in pretty "switch"
            <+> parens (pretty "Number" <> parens (pretty n))
            <> braces
              ( nested
                  ( hardline
                      <> sepMap hardline aux (zip ix es)
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
    (Raise eff args) -> pretty "_raise" <> parens (dquotes (pretty eff) <> comma <> sepMap comma pretty args)
    (Exit n) -> pretty "console.log" <> parens (pretty n)

translFn :: Fn -> Doc ann
translFn (Fn n xs bs e) =
  pretty "function"
    <+> pretty n
    <> parens (sepMap comma pretty xs)
    <> braces
      ( nested
          ( hardline
              <> sepMap hardline translBinding bs
              <> hardline
              <> translExpr e
          )
          <> hardline
      )

nested :: Doc ann -> Doc ann
nested = nest 2

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> hardline <> b
