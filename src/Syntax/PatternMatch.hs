{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Syntax.PatternMatch where

import Data.List (nub)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceWith)
import qualified Lambda.Lambda as L
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Syntax.AST
import qualified Syntax.Constant as Constant
import Syntax.DefinitionInfo
import Syntax.Primitive as Primitive
import Util.CompileEnv

data Row = Row [Pattern] [(Binder, Occur)] Expr deriving (Show)

type Matrix =
  [Row]

data DescisionTree
  = Success [(Binder, Occur)] !Expr
  | Fail
  | Switch !Occur ![(PatternSign, DescisionTree)] !(Maybe DescisionTree)
  | Swap !Int !DescisionTree

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 50 1.0})

instance Show DescisionTree where
  show tree = renderDoc (pretty tree)

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

-- swapColumn :: Int -> Int -> Matrix -> Matrix
-- swapColumn a b = map (swap a b)
--   where
--     swap i j (Row row binding expr) =
--       let (l, r) = if i > j then (j, i) else (i, j)
--           (part1, r1) = splitAt (l - 1) row
--           (a : part2, b : part3) = splitAt (r - l) r1
--        in Row (part1 ++ [b] ++ part2 ++ [a] ++ part3) expr

data Occur
  = Itself
  | Inside Int Occur
  deriving (Eq, Ord, Show)

data PatternSign
  = ConstrSign Constr
  | ConstSign Constant.Constant
  deriving (Eq, Ord, Show)

columnSigns :: Int -> Matrix -> [PatternSign]
columnSigns i = nub . concatMap (\(Row ps _ _) -> h (ps !! (i - 1)))
  where
    h (PatVar {}) = []
    h (PatConstr c _) = [ConstrSign c]
    h (PatConstant c) = [ConstSign c]
    h (PatTuple {}) = []
    h (PatOr p1 p2) = h p1 ++ h p2
    h PatWildCard = []

specialize :: Occur -> PatternSign -> Int -> Matrix -> Matrix
specialize occur sign arity = concatMap f
  where
    f (Row (pat : ps) binding expr) =
      case pat of
        (PatConstr c qs)
          | sign == ConstrSign c -> [Row (qs ++ ps) binding expr]
          | otherwise -> []
        (PatConstant c)
          | sign == ConstSign c -> [Row ps binding expr]
          | otherwise -> []
        (PatTuple qs) -> [Row (qs ++ ps) binding expr]
        PatWildCard -> [Row (replicate arity PatWildCard ++ ps) binding expr]
        (PatVar binder) -> [Row (replicate arity PatWildCard ++ ps) ((binder, occur) : binding) expr]
        PatOr q1 q2 ->
          specialize occur sign arity [Row (q1 : ps) binding expr]
            ++ specialize occur sign arity [Row (q2 : ps) binding expr]
    f _ = error ""

default' :: Occur -> Matrix -> Matrix
default' occur = concatMap f
  where
    f (Row (pat : ps) binding expr) =
      case pat of
        (PatConstr {}) -> []
        (PatConstant {}) -> []
        (PatTuple {}) -> []
        PatWildCard -> [Row ps binding expr]
        (PatVar binder) -> [Row ps ((binder, occur) : binding) expr]
        (PatOr q1 q2) -> default' occur [Row (q1 : ps) binding expr] ++ default' occur [Row (q2 : ps) binding expr]

toDescisionTree :: Map String DataTypeInfo -> Map String ConstrInfo -> [Occur] -> Matrix -> DescisionTree
toDescisionTree datatypeMap constrMap occurs = \case
  [] -> Fail
  matrix@(Row ps binding expr : _)
    | all isWildCard ps -> Success binding expr
    | otherwise -> -- TODO: use some heuristics to choose a column
        case occurs of
          (headOccur : tailOccurs) ->
            let signs = columnSigns 1 matrix
                processConstr sign =
                  let arity = signArity sign
                      occurs' = map (`Inside` headOccur) [1 .. arity] ++ tailOccurs
                      matrix' = specialize headOccur sign arity matrix
                   in go occurs' matrix'
                subTrees = map processConstr signs
                defaultTree = go tailOccurs (default' headOccur matrix)
             in Switch
                  headOccur
                  (zip signs subTrees)
                  (if exhaustive signs then Nothing else Just defaultTree)
          _ -> error "occurs is empty"
  where
    go = toDescisionTree datatypeMap constrMap
    signArity = \case
      ConstrSign c -> let (ConstrInfo arity _) = fromJust $ Map.lookup c constrMap in arity
      ConstSign _ -> 0
    isWildCard = \case
      PatWildCard -> True
      _ -> False
    -- This function need more argument like `datatype` to preform exhaustive check.
    -- Currently, it returns False when the input `signs` is empty.
    -- This behavior seems correct for most cases except for the Void type:
    --
    --   case (a : Void) of
    --
    -- will be tranlated to:
    --
    --  case (a : Void) of _ -> Abort "non-exhaustive pattern"
    --
    exhaustive = \case
      [] -> False
      (ConstSign {} : _) -> False
      signs@(ConstrSign constr : _) ->
        let (ConstrInfo _ datatype) = fromJust $ Map.lookup constr constrMap
            (DataTypeInfo allConstrs) = fromJust $ Map.lookup datatype datatypeMap
         in length signs == length allConstrs

treeToExpr :: (Expr -> CompEnv L.Expr) -> Map Occur Name -> DescisionTree -> CompEnv L.Expr
treeToExpr kont baseMap tree =
  let go = treeToExpr kont
   in case tree of
        (Success bindings expr) -> do
          expr' <- kont expr
          wrapBindings baseMap bindings expr'
        Fail -> pure $ L.PrimOp Abort [L.Const $ Constant.String "pattern match(es) are non-exhaustive"]
        (Switch occur branches fallback) ->
          case occur of
            Itself | Just itselfName <- Map.lookup Itself baseMap -> do
              branches' <- mapM (\(x, y) -> (signToConst x,) <$> go baseMap y) branches
              fallback' <- mapM (go baseMap) fallback
              pure $ L.Switch (L.Var itselfName) branches' fallback'
            (Inside i baseOccur) | Just baseName <- Map.lookup baseOccur baseMap -> do
              binder <- fresh
              let baseMap' = Map.insert occur binder baseMap
              branches' <- mapM (\(x, y) -> (signToConst x,) <$> go baseMap' y) branches
              fallback' <- mapM (go baseMap') fallback
              pure $ L.Let binder (L.Select i (L.Var baseName)) (L.Switch (L.Var binder) branches' fallback')
  where
    signToConst (ConstrSign c) = Constant.String c
    signToConst (ConstSign c) = c

    wrapBindings :: Map Occur Name -> [(Binder, Occur)] -> L.Expr -> CompEnv L.Expr
    wrapBindings _ [] expr = pure expr
    wrapBindings baseMap' ((binder, occur) : binding) expr =
      case occur of
        (Inside i baseOccur) | Just base <- Map.lookup baseOccur baseMap' -> do
          let binder' = synName binder
          wrapBindings (Map.insert occur binder' baseMap') binding (L.Let binder' (L.Select i (L.Var base)) expr)
        Itself | Just name <- Map.lookup Itself baseMap' -> do
          let binder' = synName binder
          wrapBindings baseMap' binding (L.Let binder' (L.Var name) expr)
        _ -> error $ "base occur " ++ show occur ++ " not found"

-- transl :: Map String DataTypeInfo -> Map String ConstrInfo -> Expr -> Expr
-- transl datatypes constrs e@(Match cond pats exprs) =
--   let matrix = tracePShow "matrix_initial" (zipWith (\p e -> Row [p] [] e) pats exprs)
--    in let tree = tracePShow "final tree" $ toDescisionTree datatypes constrs [Itself] matrix
--        in Const (Constant.Integer $ testTree tree)

transl :: Map String DataTypeInfo -> Map String ConstrInfo -> (Expr -> CompEnv L.Expr) -> Expr -> CompEnv L.Expr
transl datatypes constrs kont = \case
  (Match cond pats exprs) -> do
    binder <- freshStr "$binder"
    let matrix = tracePShow "matrix initail" $ zipWith (\p e -> Row [p] [] e) pats exprs
    let tree = tracePShow "final tree" $ toDescisionTree datatypes constrs [Itself] matrix
    expr <- treeToExpr kont (Map.fromList [(Itself, binder)]) tree
    L.Let binder <$> kont cond <*> pure expr

-- (Fun pat expr) ->
-- (Let pat expr1 expr2) ->

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
