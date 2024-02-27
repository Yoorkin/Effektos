{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module CPS where

import CompileEnv
import qualified Constant as C
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Primitive

data Value
  = Var Name
  | I32 Int
  | Unit
  | Tuple [Name]
  | Cont (Maybe Name) Name Term -- env x e
  | Fn Name (Maybe Name) [Name] Term -- k env x e
  deriving (Eq, Ord, Read, Data)

type Effect = String

type Cont = Name

type Function = Name

type Argument = Name

type Closure = Name

data Term
  = LetVal Name Value Term
  | LetSel Name Int Name Term -- TODO: Value to String, the Value should be (Var x)
  | LetCont Name (Maybe Name) Name Term Term
  | LetFns [(Name, Value)] Term
  | Continue Name (Maybe Name) Name
  | Apply Function Cont (Maybe Closure) [Argument]
  | LetPrim Name Primitive [Name] Term
  | Switch Name [C.Constant] [Term]
  | Handle Term [(Effect, Function)]
  | Raise Effect Cont [Argument] -- h k x
  | Halt Name
  deriving (Eq, Ord, Read, Data)

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show Value where
  show = renderDoc . pretty

instance Show Term where
  show = renderDoc . pretty

instance Plated Value where
  plate = uniplate

instance Plated Term where
  plate = uniplate

(</>) :: Doc ann -> Doc ann -> Doc ann
(</>) a b = a <> line <> b

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> hardline <> b

nested :: Doc ann -> Doc ann
nested = nest 2

instance Pretty Value where
  pretty (Var n) = pretty n
  pretty (I32 i) = pretty "i32" <+> pretty i
  pretty Unit = pretty "()"
  pretty (Tuple es) = lparen <> concatWith (\x y -> x <> comma <+> y) (map pretty es) <> rparen
  pretty (Cont env n t) = group (pretty "Cont" <> braces (pretty env) <+> pretty n <> pretty "->" <> pretty t)
  pretty (Fn k env x t) = group (parens $ pretty "Fun" <+> pretty k <+> braces (pretty env) <+> pretty x <+> pretty "->" <> nested (line <> pretty t))

instance Pretty Term where
  pretty = term2doc []

sepMapBy :: Doc ann -> (a -> Doc ann) -> [a] -> Doc ann
sepMapBy sep' f xs = concatWith (\a b -> a <> sep' <> b) (map f xs)

term2doc :: [Doc ann] -> Term -> Doc ann
term2doc bindings = \case
  (LetVal n (Fn k env xs e1) e2) ->
    let funDoc =
          pretty "Fun"
            <+> pretty n
            <> parenList2doc
              ( [ pretty k,
                  closure2doc env
                ]
                  ++ map pretty xs
              )
            <+> pretty "->"
            <> nested (line <> pretty e1)
     in term2doc (funDoc : bindings) e2
  (LetCont k env x c t) ->
    let contDoc =
          group
            ( pretty "Cont"
                <+> ( pretty k
                        <> parenList2doc
                          [ closure2doc env,
                               pretty x
                          ]
                        <+> pretty "="
                        <> nested
                          ( line
                              <> pretty c
                          )
                    )
            )
     in term2doc (contDoc : bindings) t
  (LetVal n v t) ->
    group
      ( pretty
          "let"
          <> nested
            ( line
                <> pretty n
                <+> pretty "="
                <> group (nested (line <> pretty v))
            )
            </> pretty "in"
      )
      </> term2doc bindings t
  (LetSel n i n' t) ->
    pretty "let"
      <+> pretty n
      <+> group
        ( nested
            ( pretty "="
                </> ( pretty n' <> brackets (pretty i)
                    )
            )
            </> pretty "in"
        )
        </> term2doc bindings t
  (LetPrim n op ns t) ->
    group
      ( pretty
          "let"
          <> nested
            ( line
                <> pretty n
                <+> pretty "="
                <> group
                  ( nested
                      ( line
                          <> pretty (show op)
                          <+> lparen
                          <> concatWith (\x y -> x <> comma <+> y) (map pretty ns)
                          <> rparen
                      )
                  )
            )
            </> pretty "in"
      )
      </> term2doc bindings t
  (LetFns fns l) -> group (pretty "Letrec" <> nest 2 (line <> concatWith (</>) (map g fns)) </> pretty "in" </> pretty l)
    where
      g (n, v) = group (pretty n <+> pretty "=" <+> pretty v)
  (Switch n ix ks) ->
    pretty "switch"
      <+> pretty n
      <> colon
      <> nested (hardline <> sepMapBy hardline f (zip ix ks))
      <> bindings2doc bindings
    where
      f (i, e) = pretty (show i) <+> pretty "->" <+> align (pretty e)
  (Halt e) ->
    group (pretty "halt" </> pretty e)
  -- <> line
  -- <> bindings2doc bindings
  (Handle e hdls) ->
    group (pretty "handle" <> nested (line <> pretty e) </> pretty "with" <> nested (line <> pretty hdls))
  -- <> line
  -- <> bindings2doc bindings
  (Raise h k xs) ->
    group (pretty "Raise" <+> pretty h <+> pretty k <+> pretty xs)
  -- <> line
  -- <> bindings2doc bindings
  (Continue k env x) ->
    group (pretty "continue" <+> pretty k <+> parenList2doc [closure2doc env, pretty x])
      <> bindings2doc bindings
  (Apply f k env xs) ->
    group (pretty "call" <+> pretty f <+> parenList2doc ([pretty k, closure2doc env] ++ map pretty xs))
      <> bindings2doc bindings
 where
    bindings2doc [] = mempty
    bindings2doc bindings = group (hardline <> pretty "where" <> nest 2 (line <> vsep bindings))
    closure2doc Nothing = pretty "()"
    closure2doc (Just x) = pretty x
    parenList2doc xs = parens (concatWith (\a b -> a <> comma <+> b) xs)
