{-# LANGUAGE LambdaCase #-}

module PatternMatch where

import qualified Constant
import Data.List (findIndex, nub)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (traceWith)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Syntax
import Text.Pretty.Simple (pShow)

data Row = Row [Pattern] Expr deriving (Show)

type Matrix =
  [Row]

swapColumn :: Int -> Int -> Matrix -> Matrix
swapColumn a b = map (swap a b)
  where
    swap i j (Row row expr) =
      let (l, r) = if i > j then (j, i) else (i, j)
          (part1, r1) = splitAt (l - 1) row
          (a : part2, b : part3) = splitAt (r - l) r1
       in Row (part1 ++ [b] ++ part2 ++ [a] ++ part3) expr

data Occur = Nil | Inside Int Occur deriving (Show)

specialize :: Constr -> Int -> Matrix -> Matrix
specialize c arity = concatMap f
  where
    f (Row (pat : ps) expr) =
      case pat of
        (PatConstr c' qs)
          | c == c' ->
               [Row (qs ++ ps) expr]
          | c /= c' -> []
        PatWildCard -> [Row (replicate arity PatWildCard ++ ps) expr]
        PatOr q1 q2 ->
          specialize c arity [Row (q1 : ps) expr]
            ++ specialize c arity [Row (q2 : ps) expr]
    f _ = error ""

default' :: Matrix -> Matrix
default' = concatMap f
  where
    f (Row (pat : ps) expr) =
      case pat of
        (PatConstr {}) -> []
        PatWildCard -> [Row ps expr]
        (PatOr q1 q2) -> default' [Row (q1 : ps) expr] ++ default' [Row (q2 : ps) expr]

data DescisionTree
  = Success !Expr
  | Fail
  | Switch !Occur ![(Constr, DescisionTree)] !(Maybe DescisionTree)
  | Swap !Int !DescisionTree

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show DescisionTree where
  show tree = renderDoc (pretty tree)

instance Pretty DescisionTree where
  pretty (Success e) = pretty "Success" <+> nest 2 (pretty $ show e)
  pretty (Fail) = pretty "Fail"
  pretty (Switch occur subtrees fallback) =
    pretty "Switch"
      <+> parens (pretty (show occur))
      <> pretty ":" 
      <> nest 4
        ( (if length subtrees == 0 then mempty else line) <> vcat (map (\(c, t) -> pretty c <+> pretty "->" <+> pretty t) subtrees)
            <> case fallback of Nothing -> mempty; Just x -> line <> pretty "* ->" <+> pretty x
        )

getColumnConstrs :: Int -> Matrix -> [Constr]
getColumnConstrs i = nub . concatMap (\(Row ps _) -> h (ps !! (i - 1)))
  where
    h (PatVar {}) = []
    h (PatConstr c _) = [c]
    h (PatConstant {}) = []
    h (PatTuple {}) = []
    h (PatOr p1 p2) = h p1 ++ h p2
    h PatWildCard = []

type ConstrArity = M.Map Constr Int

matrixToDescisionTree :: ConstrArity -> [Occur] -> Int -> Matrix -> DescisionTree
matrixToDescisionTree arityMap occurs width matrix =
  case matrix of
    [] -> Fail
    (Row ps expr : _)
      | all isWildCard ps -> Success expr
      | otherwise -> -- it can use some heuristics to choose a column in feature
          let constrs = head columnConstrs
           in let processConstr constr =
                    let arity = fromJust $ M.lookup constr arityMap
                     in let occurs' = map (`Inside` head occurs) [1 .. arity] ++ tail occurs
                         in let matrix' = specialize constr arity matrix
                             in matrixToDescisionTree arityMap occurs' width matrix'
               in let subTrees = zip constrs (map processConstr constrs)
                   in let defaultTree =
                            let occurs' = tail occurs
                             in let matrix' = default' matrix
                                 in matrixToDescisionTree arityMap occurs' width matrix'
                       in Switch (head occurs) subTrees (Just defaultTree)
  where
    isWildCard = \case
      PatWildCard -> True
      _ -> False
    columnConstrs = map (`getColumnConstrs` matrix) [1 .. width]

constrArityMap :: [Definition] -> M.Map Constr Int
constrArityMap defs = M.fromList $ concatMap f defs
  where
    f (Data _ constrs) = concatMap g constrs
    f _ = []
    g (constr, tys) = [(constr, length tys)]

update :: [a] -> Int -> (a -> [a]) -> [a]
update xs i f =
  let (xs, y : ys) = splitAt (i - 1) xs
   in xs ++ f y ++ ys

transl :: Expr -> [Definition] -> Expr
transl e@(Match cond pats exprs) defs =
  let matrix = tracePShow "matrix_initial" (zipWith (\p e -> Row [p] e) pats exprs)
      arityMap = constrArityMap defs
   in let tree = tracePShow "final tree" $ matrixToDescisionTree arityMap [Nil] 1 matrix
       in Const (Constant.Integer $ testTree tree)

tracePShow s = traceWith (((++) "------------------------------\n") . 
  ((++) ("==========" ++ s ++ "=============\n")) . show)

testTree :: DescisionTree -> Int
testTree t = case t of
  Success {} -> 0
  Fail -> 0
  (Switch _ xs _) -> sum (map (testTree . snd) xs)
