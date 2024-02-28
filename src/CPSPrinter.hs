{-# LANGUAGE LambdaCase #-}

module CPSPrinter where

import CPS (Term (..), Value (..))
import CompileEnv
import Constant ()
import qualified Data.Map.Lazy as Map
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Primitive ()
import Util (occurCount)
import Data.Maybe (isNothing)

sepBy :: Doc a -> [Doc a] -> Doc a
sepBy s = concatWith (\x acc -> x <> s <> acc)

sepMap :: Doc a -> (b -> Doc a) -> [b] -> Doc a
sepMap s f = sepBy s . map f

closure2doc :: Maybe Name -> Doc a
closure2doc = \case
  Nothing -> pretty "()"
  (Just x) -> pretty (show x)

value2doc :: Map.Map Name Int -> Value -> Doc a
value2doc occurs = \case
  (Var n) -> pretty (show n)
  (I32 i) -> pretty i
  CPS.Unit -> pretty "()"
  (Tuple xs) -> parens (sepMap (comma <> space) pretty xs)
  (Cont env n t) ->
    group
      ( pretty "Cont"
          <> parens (sepBy (comma <> space) 
            ((case env of Nothing -> []; _ -> [closure2doc env]) ++  [pretty n]))
          <+> pretty "->"
          <> nest 2 (line <> term2doc occurs Map.empty t)
      )
  (Fn k env xs t) ->
    group
      ( pretty "Fun"
          <> parens
            ( sepBy
                (comma <> space)
                ([pretty k] ++ (case env of Nothing -> []; _ -> [closure2doc env]) ++ map pretty xs)
            )
          <+> pretty "->"
          <> nest 2 (line <> term2doc occurs Map.empty t)
      )

term2doc :: Map.Map Name Int -> Map.Map Name Value -> Term -> Doc a
term2doc occurs bindings expr =
  let go = term2doc occurs Map.empty
   in case expr of
        (LetVal n fn@(Fn {}) e2) ->
          term2doc occurs (Map.insert n fn bindings) e2
        (LetCont k env x c t) ->
          let cont = Cont env x c
           in term2doc occurs (Map.insert k cont bindings) t
        (LetVal n v t) ->
          group
            ( pretty
                "let"
                <> nest
                  2
                  ( line
                      <> pretty n
                      <+> pretty "="
                      <> group (nest 2 (line <> value2doc occurs v))
                  )
                <> line
                <> pretty "in"
            )
            <> line
            <> term2doc occurs bindings t
        (LetSel n i n' t) ->
          pretty "let"
            <+> pretty n
            <+> group
              ( nest
                  2
                  ( pretty "="
                      <> line
                      <> ( pretty n' <> brackets (pretty i)
                         )
                  )
                  <> line
                  <> pretty "in"
              )
            <> line
            <> term2doc occurs bindings t
        (LetPrim n op ns t) ->
          group
            ( pretty
                "let"
                <> nest
                  2
                  ( line
                      <> pretty n
                      <+> pretty "="
                      <> group
                        ( nest
                            2
                            ( line
                                <> pretty (show op)
                                <+> lparen
                                <> concatWith (\x y -> x <> comma <+> y) (map pretty ns)
                                <> rparen
                            )
                        )
                  )
                <> line
                <> pretty "in"
            )
            <> line
            <> term2doc occurs bindings t
        (LetFns fns l) ->
          group
            ( pretty "Letrec"
                <> nest 2 (line <> concatWith (\a b -> a <> line <> b) (map g fns))
                <> line
                <> pretty "in"
                <> line
                <> go l
            )
          where
            g :: (Name, Value) -> Doc ann
            g (n, v) = group (pretty n <+> pretty "=" <+> value2doc occurs v)
        (Switch n ix ks) ->
          pretty "case"
            <+> pretty n
            <+> pretty "of"
            <> nest 2 (hardline <> sepMap hardline f (zip ix ks))
            <> bindings2doc occurs bindings
          where
            f (i, e) = pretty (show i) <+> pretty "->" <+> align (go e)
        (Halt e) ->
          group (pretty "halt" <> line <> pretty e)
            <> bindings2doc occurs bindings
        (Handle e hdls) ->
          group
            ( pretty "handle"
                <> nest 2 (line <> go e)
                <> line
                <> pretty "with"
                <> nest 2 (line <> pretty hdls)
            )
        -- <> line
        -- <> bindings2doc bindings
        (Raise h k xs) ->
          group (pretty "Raise" <+> pretty h <+> pretty k <+> pretty xs)
        -- <> line
        -- <> bindings2doc bindings
        (Continue k env x) ->
          case (Map.lookup k occurs, Map.lookup k bindings) of
            (Just 2, Just (Cont env' x' t)) ->
              group
                ( pretty "let*"
                    <+> (case env' of Nothing -> pretty x'; _ -> parens (closure2doc env' <> comma <+> pretty x'))
                    <+> pretty "="
                    <+> (case env of Nothing -> pretty x; _ -> parens (closure2doc env <> comma <+> pretty x))
                    <+> pretty "in"
                    <> line
                    <> term2doc occurs (Map.delete k bindings) t
                )
            _ ->
              group
                ( pretty "continue"
                    <+> pretty k
                    <> parens (sepBy (comma <> space) 
                     ((case env of Nothing -> []; _ -> [closure2doc env]) ++ [pretty x]))
                )
                <> bindings2doc occurs bindings
        (Apply f k env xs) ->
          case (Map.lookup k occurs, Map.lookup k bindings) of
            (Just 2, Just (Cont env' x' t)) ->
              pretty "let*"
                <+> (case env' of Nothing -> pretty x'; _ -> parens (closure2doc env' <> comma <+> pretty x'))
                <+> pretty "="
                <+> pretty f
                <> parens (sepBy (comma <> space) ((if isNothing env then [] else [closure2doc env]) ++ map pretty xs))
                <> line
                <> term2doc occurs (Map.delete k bindings) t
            _ ->
              group
                ( pretty "call"
                    <+> pretty f
                    <+> parens (sepBy (comma <> space) ([pretty k, closure2doc env] ++ map pretty xs))
                )
                <> bindings2doc occurs bindings

bindings2doc :: Map.Map Name Int -> Map.Map Name Value -> Doc ann
bindings2doc occurs bindings = if Map.null bindings then mempty else group (hardline <> pretty "where" <> nest 2 (line <> vsep (map f (Map.toList bindings))))
  where
    f (n, v) = group (pretty n <+> pretty "=" <+> value2doc occurs v)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

prettyCPS :: Term -> String
prettyCPS term = show occurs ++ "\n" ++ (renderDoc . term2doc occurs Map.empty $ term)
  where
    occurs = occurCount term
