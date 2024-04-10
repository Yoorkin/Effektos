{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Typing.PatternMatch where

import Common.CompileEnv
import Common.Name
import Data.List (nub)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceWith)
import Prettyprinter
import qualified Syntax.Constant as Constant
import qualified Syntax.Primitive as Primitive
import Typing.Builtin (unitType)
import Typing.Typedtree
import Util.Prettyprint

data Row = Row [Pattern] [(Binder, Occur)] Expr deriving (Show)

type Matrix =
  [Row]

data DescisionTree
  = Success [(Binder, Occur)] Expr
  | Fail
  | Switch Occur [(Pattern, DescisionTree)] (Maybe DescisionTree)
  | Swap Int DescisionTree

instance Show DescisionTree where
  show = render . pretty

instance Pretty DescisionTree where
  pretty (Success bs e) = pretty "Success" <+> pretty (show bs) <+> align (pretty $ show e)
  pretty Fail = pretty "Fail"
  pretty (Switch occur subtrees fallback) =
    pretty "Switch"
      <+> parens (pretty (show occur))
      <> pretty ":"
      <> nest
        4
        ( (if null subtrees then mempty else line)
            <> vcat (map (\(c, t) -> pretty (show c) <+> pretty "->" <+> pretty t) subtrees)
            <> case fallback of Nothing -> mempty; Just x -> line <> pretty "* ->" <+> pretty x
        )

data Occur
  = Itself
  | Inside Int Occur
  deriving (Eq, Ord, Show)

equalSign :: Pattern -> Pattern -> Bool
equalSign (PatLit _ c1) (PatLit _ c2) = c1 == c2
equalSign (PatCon _ c1 _) (PatCon _ c2 _) = c1 == c2
equalSign _ _ = False

breakType :: Type -> [Type]
breakType (TypeConstr _ xs) = xs
breakType _ = []

specialize :: Occur -> Pattern -> Matrix -> Matrix
specialize occur sign = concatMap go
  where
    go (Row (pat : ps) binding expr) = case pat of
      PatCon _ _ qs
        | equalSign pat sign -> [Row (qs ++ ps) binding expr]
        | otherwise -> []
      PatLit _ _
        | equalSign pat sign -> [Row ps binding expr]
        | otherwise -> []
      PatWildCard ty -> [Row (map PatWildCard (breakType ty) ++ ps) binding expr]
      PatVar ty var -> [Row (map PatWildCard (breakType ty) ++ ps) ((var, occur) : binding) expr]
      PatOr _ q1 q2 ->
        specialize occur sign [Row (q1 : ps) binding expr]
          ++ specialize occur sign [Row (q2 : ps) binding expr]
    go _ = error ""

default' :: Occur -> Matrix -> Matrix
default' occur = concatMap go
  where
    go (Row (pat : ps) binding expr) = case pat of
      PatCon {} -> []
      PatLit {} -> []
      PatWildCard _ -> [Row ps binding expr]
      PatVar _ var -> [Row ps ((var, occur) : binding) expr]
      PatOr _ q1 q2 ->
        default' occur [Row (q1 : ps) binding expr]
          ++ default' occur [Row (q2 : ps) binding expr]

toDescisionTree :: Map Name DatatypeInfo -> [Occur] -> Matrix -> DescisionTree
-- If pattern columns is empty, the pattern matching always fail.
toDescisionTree _  _ [] = Fail
-- The pattern matching is first match semantic.
-- Therefore, if patterns in the first row are WildCards, it succeed.
toDescisionTree _  _ (Row pats bindings expr : _)
  | all isWildCard pats = Success bindings expr
-- Otherwise, collecting tesing conditions (signs) from a column and generate a Switch.
toDescisionTree datatypes (testingOccur : remainOccurs) matrix =
  Switch testingOccur (zip patterns subtrees) defaultTree
  where
    patterns = signPatternsInColumn 1 matrix
    subtrees = map go patterns

    go testedPat =
      let arity = patternArity testedPat
          remainOccurs' = [Inside i testingOccur | i <- [1 .. arity]] ++ remainOccurs
          matrix' = specialize testingOccur testedPat matrix
       in toDescisionTree datatypes remainOccurs' matrix'
    
    testingTy = typeOfPat (case head matrix of Row (p:_) _ _ -> p)
    defaultTree =
      if isExhaustive datatypes testingTy patterns
        then Nothing
        else Just $ toDescisionTree datatypes remainOccurs (default' testingOccur matrix)

signPatternsInColumn :: Int -> Matrix -> [Pattern]
signPatternsInColumn i = nub . concatMap (\(Row ps _ _) -> go (ps !! (i - 1)))
  where
    go (PatVar _ _) = []
    go (PatCon ty c xs) = [PatCon ty c xs]
    go (PatLit ty c) = [PatLit ty c]
    go (PatOr _ p1 p2) = go p1 ++ go p2
    go (PatWildCard _) = []

isWildCard :: Pattern -> Bool
isWildCard = \case
  PatWildCard _ -> True
  _ -> False

patternArity :: Pattern -> Int
patternArity (PatCon _ _ xs) = length xs
patternArity _ = 0

isExhaustive :: Map Name DatatypeInfo -> Type -> [Pattern] -> Bool
isExhaustive datatypes (TypeConstr n _) pats =
  case Map.lookup n datatypes of
    Just (DataTypeInfo {datatypeConstrs}) -> length datatypeConstrs == length pats
    _ -> False
isExhaustive _ _ _ = False


translate :: Map Name DatatypeInfo -> Program -> Program
translate datatypes (Program bindings) = Program (map goBinding bindings)   
  where 
    goBinding (TopBinding n e) = TopBinding n (go e)
    go e@(Case ty cond pats exprs) = 
        let matrix = [Row [p] [] e | p <- pats, e <- exprs] 
            tree = toDescisionTree datatypes [Itself] matrix
         in traceDocWith (\e -> pretty $ show tree) e
    go (Lam ty p e) = Lam ty p (go e)
    go (App ty e1 e2) = App ty (go e1) (go e2)
    go (Let ty p e1 e2) = Let ty p (go e1) (go e2)
    go e = e


tracePShow :: (Show a) => [Char] -> a -> a
tracePShow s =
  traceWith
    ( (++) "------------------------------\n"
        . (++) ("==========" ++ s ++ "=============\n")
        . show
    )

testTree :: DescisionTree -> Int
testTree t = case t of
  Success {} -> 0
  Fail -> 0
  (Switch _ xs _) -> sum (map (testTree . snd) xs)
