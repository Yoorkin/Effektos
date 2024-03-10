{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Core.CPSPrinter where

import Core.CPS as CPS
import qualified Data.Map.Lazy as Map
import Prettyprinter
import Util.CompileEnv
import Util.Prettyprint
import Util.Utils (occurCount)

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
  (Cont n t) ->
    group
      ( pretty "Cont"
          <> parens
            ( sepBy
                (comma <> space)
                [pretty n]
            )
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
        (LetCont k x c t) ->
          let cont = Cont x c
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
                <> nest 2 (line <> concatWith (\a b -> a <> line <> line <> b) (map g fns))
                <> line
                <> pretty "in"
                <> line
                <> go l
            )
          where
            g :: (Name, Value) -> Doc ann
            g (n, v) = group (pretty n <+> pretty "=" <+> value2doc occurs v)
        (LetConts conts l) ->
          group
            ( pretty "Letconts"
                <> nest 2 (line <> concatWith (\a b -> a <> line <> b) (map g conts))
                <> line
                <> pretty "in"
                <> line
                <> go l
            )
          where
            g :: (Name, Value) -> Doc ann
            g (n, v) = group (pretty n <+> pretty "=" <+> value2doc occurs v)
        (Switch n ix ks fb) ->
          pretty "case"
            <+> pretty n
            <+> pretty "of"
            <> nest 2 (hardline <> sepBy hardline (zipWith (curry f) ix ks ++ fbDoc))
            <> bindings2doc occurs bindings
          where
            fbDoc = case fb of
              Nothing -> []
              Just x -> [pretty "_ ->" <+> nest 2 (hardline <> go x)]
            f (i, e) = pretty (show i) <+> pretty "->" <+> nest 2 (hardline <> go e)
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
            <> bindings2doc occurs bindings
        (Raise h k xs) ->
          group (pretty "Raise" <+> pretty h <+> pretty k <+> pretty xs)
        -- <> line
        -- <> bindings2doc bindings
        (Continue k x) ->
          case (Map.lookup k occurs, Map.lookup k bindings) of
            -- (Just 2, Just (Cont x' t)) ->
            --   group
            --     (
            --       pretty "continue"
            --         <+> parens (pretty x)
            --         <+> pretty "$" <+> pretty "Cont" <+> pretty k
            --         <> parens (pretty x')
            --         <+> pretty "->"
            --         <> line
            --         <> term2doc occurs (Map.delete k bindings) t
            --     )
            _ ->
              group
                ( pretty "continue"
                    <+> pretty k
                    <> parens
                      ( sepBy
                          (comma <> space)
                          [pretty x]
                      )
                )
                <> bindings2doc occurs bindings
        (Apply f k env xs) ->
          case (Map.lookup k occurs, Map.lookup k bindings) of
            -- (Just 2, Just (Cont x' t)) ->
            --   pretty "" <> pretty f
            --     <+> sepBy (comma <> space) (map pretty xs)
            --     <+> pretty "$" <+> pretty "Cont" <+> pretty k
            --     <> parens (parens (pretty x'))
            --     <+> pretty "->"
            --     <> line
            --     <> term2doc occurs (Map.delete k bindings) t
            _ ->
              group
                ( pretty "call"
                    <+> pretty f
                    <+> parens (sepBy (comma <> space) ([pretty k, closure2doc env] ++ map pretty xs))
                )
                <> bindings2doc occurs bindings

bindings2doc :: Map.Map Name Int -> Map.Map Name Value -> Doc ann
bindings2doc occurs bindingMap =
  let contsDoc = if conts /= [] then line <> sepBy hardline (map cont2doc conts) else mempty
      fnsDoc = if fns /= [] then line <> pretty "where" <> nest 2 (line <> hsep (map fn2doc fns)) else mempty
   in contsDoc <> fnsDoc
  where
    fn2doc (n, v) = group (pretty n <+> pretty "=" <+> value2doc occurs v)
    cont2doc (n, Cont x t) = pretty "----" <> pretty n <> parens (pretty x) <> pretty ":" <> line <> term2doc Map.empty Map.empty t
    isCont = \case
      (_, Cont {}) -> True
      _ -> False
    isFn = not . isCont
    bindings = Map.toList bindingMap
    conts = filter isCont bindings
    fns = filter isFn bindings

prettyCPS :: Term -> String
prettyCPS term = show occurs ++ "\n" ++ (render . term2doc occurs Map.empty $ term)
  where
    occurs = occurCount term
