{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ASM.Liveness where

import ASM.CFG
import ASM.RTL hiding (Label)
import Data.List (union, (\\))
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust, mapMaybe)
import Util.Prettyprint (render)
import Prettyprinter

type Operands = [Operand]

type InstrLiveness = (Operands,Operands)

emptyLiveness :: InstrLiveness
emptyLiveness = ([],[])

newtype FnLiveInfo
  = FnLiveInfo (Map.Map Label [InstrLiveness])
  deriving (Eq)

type Liveness = Map.Map String FnLiveInfo

instance Pretty FnLiveInfo where
  pretty (FnLiveInfo mp) = vsep (map f (Map.toList mp))
    where
      f (label,liveness) =
          pretty label <> pretty ":"
                 <> nest 2 (line <> vsep (map g liveness))
      g (ins,outs) = pretty (show ins) <+> pretty (show outs)


instance Show FnLiveInfo where
  show = render . pretty

type Ins = Map.Map Label [Operand]

type Outs = Map.Map Label [Operand]

livenessOfInstr :: [InstrLiveness] -> Inst -> InstrLiveness
livenessOfInstr succLiveness instr = (in', out')
  where
    out' = foldl union [] (map fst succLiveness)
    in' = gen `union` (out' \\ kill)

    gen = readed instr
    kill = written instr

    filterOut = filter $ \case
      (Val _) -> True
      (Reg _) -> True
      RLK -> True
      _ -> False

    written =
      filterOut . \case
        (Binary a _ _ _) -> [a]
        (Unary a _ _) -> [a]
        (Move a _) -> [a]
        (Load a _ _) -> [a]
        (Call _ _) -> [Reg 1]
        _ -> []
    readed =
      filterOut . \case
        (Binary _ _ b c) -> [b, c]
        (Unary _ _ b) -> [b]
        (Move _ b) -> [b]
        (Load _ b c) -> [b, c]
        (Store a b c) -> [a, b, c]
        (Goto a) -> [a]
        (Jeq a b c) -> [a, b, c]
        Ret -> [RLK]
        (Call f arity) -> f : RLK : [Reg i | i <- [1..arity]]
        _ -> []

livenessOfBlock :: FnLiveInfo -> BasicBlock -> [InstrLiveness]
livenessOfBlock (FnLiveInfo mp) (Block label instr _ succs)
  = zipWith livenessOfInstr (map (:[]) (tail liveness) ++ [succsLive]) instr
  where
   liveness = fromJust $ Map.lookup label mp
   succsLive = map head $ mapMaybe (`Map.lookup` mp) succs

livenessOfFn :: CFG -> FnLiveInfo -> FnLiveInfo
livenessOfFn (CFG _ blockMap) fnLv = FnLiveInfo $ Map.map (livenessOfBlock fnLv) blockMap

analyze' :: CFG -> FnLiveInfo
analyze' cfg@(CFG _ blockMap) = findFixPoint iterates
  where
    iterates = iterate (livenessOfFn cfg) initialFnLiveInfo
    initialFnLiveInfo = FnLiveInfo (Map.map processBlock blockMap)
    processBlock (Block _ instr _ _) = map (const emptyLiveness) instr

    findFixPoint (x : ls@(y : _))
      | x == y = x
      | x /= y = findFixPoint ls
    findFixPoint _ = undefined

analyze :: CFGs -> Map.Map String FnLiveInfo
analyze (CFGs mp) = Map.map analyze' mp
