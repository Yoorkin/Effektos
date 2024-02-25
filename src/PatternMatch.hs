{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PatternMatch where

import qualified Constant
import Data.List (findIndex, nub)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (traceWith)
import DefinitionInfo
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Syntax
import Text.Pretty.Simple (pShow)

data Row = Row [Pattern] [(Binder, Occur)] Expr deriving (Show)

type Matrix =
  [Row]

data DescisionTree
  = Success [(Binder,Occur)] !Expr
  | Fail
  | Switch !Occur ![(Constr, DescisionTree)] !(Maybe DescisionTree)
  | Swap !Int !DescisionTree

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show DescisionTree where
  show tree = renderDoc (pretty tree)

instance Pretty DescisionTree where
  pretty (Success bs e) = pretty "Success" <+> pretty (show bs) <+> nest 2 (pretty $ show e)
  pretty Fail = pretty "Fail"
  pretty (Switch occur subtrees fallback) =
    pretty "Switch"
      <+> parens (pretty (show occur))
      <> pretty ":"
      <> nest
        4
        ( (if null subtrees then mempty else line)
            <> vcat (map (\(c, t) -> pretty c <+> pretty "->" <+> pretty t) subtrees)
            <> case fallback of Nothing -> mempty; Just x -> line <> pretty "* ->" <+> pretty x
        )

-- swapColumn :: Int -> Int -> Matrix -> Matrix
-- swapColumn a b = map (swap a b)
--   where
--     swap i j (Row row binding expr) =
--       let (l, r) = if i > j then (j, i) else (i, j)
--           (part1, r1) = splitAt (l - 1) row
--           (a : part2, b : part3) = splitAt (r - l) r1
--        in Row (part1 ++ [b] ++ part2 ++ [a] ++ part3) expr

data Occur = Itself | Inside Int Occur deriving (Show)

columnConstrs :: Int -> Matrix -> [Constr]
columnConstrs i = nub . concatMap (\(Row ps _ _) -> h (ps !! (i - 1)))
  where
    h (PatVar {}) = []
    h (PatConstr c _) = [c]
    h (PatConstant {}) = []
    h (PatTuple {}) = []
    h (PatOr p1 p2) = h p1 ++ h p2
    h PatWildCard = []

specialize :: Occur -> Constr -> Int -> Matrix -> Matrix
specialize occur constr arity = concatMap f
  where
    f (Row (pat : ps) binding expr) =
      case pat of
        (PatConstr constr' qs)
          | constr == constr' ->
              [Row (qs ++ ps) binding expr]
          | otherwise -> []
        PatWildCard -> [Row (replicate arity PatWildCard ++ ps) binding expr]
        (PatVar binder) -> [Row (replicate arity PatWildCard ++ ps) ((binder, occur) : binding) expr]
        PatOr q1 q2 ->
          specialize occur constr arity [Row (q1 : ps) binding expr]
            ++ specialize occur constr arity [Row (q2 : ps) binding expr]
    f _ = error ""

default' :: Occur -> Matrix -> Matrix
default' occur = concatMap f
  where
    f (Row (pat : ps) binding expr) =
      case pat of
        (PatConstr {}) -> []
        PatWildCard -> [Row ps binding expr]
        (PatVar binder) -> [Row ps ((binder,occur):binding) expr]
        (PatOr q1 q2) -> default' occur [Row (q1 : ps) binding expr] ++ default' occur [Row (q2 : ps) binding expr]
        _ -> error (show pat)

toDescisionTree :: Map String DataTypeInfo -> Map String ConstrInfo -> [Occur] -> Matrix -> DescisionTree
toDescisionTree datatypeMap constrMap occurs = \case
  [] -> Fail
  matrix@(Row ps binding expr : _)
    | all isWildCard ps -> Success binding expr
    | otherwise -> -- TODO: use some heuristics to choose a column
        let constrs = columnConstrs 1 matrix
            headOccur = head occurs
            tailOccurs = tail occurs
            processConstr constr =
              let (ConstrInfo arity _) = fromJust $ Map.lookup constr constrMap
                  occurs' = map (`Inside` headOccur) [1 .. arity] ++ tailOccurs
                  matrix' = specialize headOccur constr arity matrix
               in go occurs' matrix'
            subTrees = map processConstr constrs
            defaultTree = go tailOccurs (default' headOccur matrix)
         in Switch
              headOccur
              (zip constrs subTrees)
              (if exhaustive constrs then Nothing else Just defaultTree)
  where
    go = toDescisionTree datatypeMap constrMap
    isWildCard = \case
      PatWildCard -> True
      _ -> False
    exhaustive = \case
      [] -> True
      constrs@(constr : _) ->
        let (ConstrInfo _ datatype) = fromJust $ Map.lookup constr constrMap
            (DataTypeInfo allConstrs) = fromJust $ Map.lookup datatype datatypeMap
         in length constrs == length allConstrs

-- treeToExpr :: Map Occur Name -> DescisionTree -> CompEnv Expr
-- treeToExpr tree =
--    case tree of
--      (Success expr) -> pure expr
--      Fail -> pure $ Prim Abort (Const $ Constant.String "pattern match(es) are non-exhaustive")
--      (Switch occur branches fallback) ->

transl :: Map String DataTypeInfo -> Map String ConstrInfo -> Expr -> Expr
transl datatypes constrs e@(Match cond pats exprs) =
  let matrix = tracePShow "matrix_initial" (zipWith (\p e -> Row [p] [] e) pats exprs)
   in let tree = tracePShow "final tree" $ toDescisionTree datatypes constrs [Itself] matrix
       in Const (Constant.Integer $ testTree tree)

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
