{-# LANGUAGE LambdaCase #-}

module ASM.RegAlloc (allocation, rewrite) where

import ASM.Graph
import ASM.Liveness (FnLiveInfo (..), Liveness)
import ASM.RTL
import qualified ASM.RTL as RTL
import Data.List (find, (\\))
import Data.Map as Map (Map, elems, empty, insert, lookup, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.OldList (nub)
import Prettyprinter
import Util.Prettyprint (traceDoc, traceDocWith)
import Prelude hiding (lookup)

type Register = Operand

type Allocation = Map Operand Register

type Solutions = Map String Allocation

getRenderList :: (Eq a, Show a) => Int -> Graph a -> [a] -> [a]
getRenderList maxReg graph acc
  | let vexs = allVertix graph,
    not (null vexs) =
      case find ((<= maxReg) . length . flip inOutVexs graph) vexs of
        Nothing -> error "alloc failed"
        Just vex -> getRenderList maxReg (removeVertix vex graph) (vex : acc)
  | otherwise = reverse acc

allocFn :: [Register] -> FnLiveInfo -> Allocation
allocFn availableReg (FnLiveInfo blocks) =
  traceDocWith
    ( \x ->
        pretty "--> render result:"
          <+> pretty (show x)
          <+> line
          <+> pretty "---------------------------"
    )
    $ render Map.empty renderList
  where
    maxReg = length availableReg

    lineConflicts = concatMap (\(a, b) -> [a, b]) $ concat $ Map.elems blocks

    conflictEdges conflicts =
      [Edge (x, y) | x <- conflicts, y <- conflicts, x /= y]

    graph =
      traceDocWith (\x -> pretty "==> graph" <> line <> pretty (show x) <> line) $
        fromVertixEdges (nub $ concat lineConflicts) (nub $ concatMap conflictEdges lineConflicts)

    renderList =
      traceDocWith (\x -> pretty "==> renderList" <> line <> pretty (show x)) $
        getRenderList maxReg graph []

    render colorMap [] = colorMap
    render colorMap (Reg _ : xs) = render colorMap xs
    render colorMap (RLK : xs) = render colorMap xs
    render colorMap (x : xs) =
      let usedRegs = flip mapMaybe (inOutVexs x graph) $ \operand ->
            case operand of
              (Val _) -> lookup operand colorMap
              (Reg _) -> Just operand
              RLK -> Just RLK
              _ -> error ""
          available = availableReg \\ usedRegs
          reg = head available
       in render (Map.insert x reg colorMap) xs

allocation :: [Register] -> Liveness -> Solutions
allocation availableRegs =
  Map.mapWithKey
    ( \k fn ->
        traceDocWith (\_ -> pretty (show k) <> colon) $
          allocFn availableRegs fn
    )

rewriteInstr :: Allocation -> [RTL.Inst] -> [RTL.Inst]
rewriteInstr allocMap = mapMaybe go
  where
    subst val@(Val _) = lookup val allocMap
    subst operand = Just operand

    f x = fromMaybe x (subst x)

    go (Binary a op b c) | Just a' <- subst a = Just $ Binary a' op (f b) (f c)
    go (Unary a op b) | Just a' <- subst a = Just $ Unary a' op (f b)
    go (Move a b) | Just a' <- subst a = Just $ Move a' (f b)
    go (Load a b c) | Just a' <- subst a = Just $ Load a' (f b) (f c)
    go (Store a b c) = Just $ Store (f a) (f b) (f c)
    go (Goto a) = Just $ Goto (f a)
    go (Jeq a b c) = Just $ Jeq (f a) (f b) (f c)
    go (Call a arity) = Just $ Call (f a) arity
    go (NewBlock l) = Just $ NewBlock l
    go Ret = Just Ret
    go x = traceDoc (pretty "(*) discard instr:" <+> pretty (show x)) Nothing

rewrite :: Solutions -> RTL.Program -> RTL.Program
rewrite solutions (RTL.Program fns instr) = RTL.Program fns' instr'
  where
    fns' = map (\(Fn str fnInstr) -> Fn str (rewriteInstr (solutions ! str) fnInstr)) fns
    instr' = rewriteInstr (solutions ! "main") instr
