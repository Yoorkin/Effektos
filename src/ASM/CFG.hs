{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ASM.CFG where

import qualified ASM.RTL as RTL
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)

type Label = RTL.Label

type In = Label

type Out = Label

data BasicBlock
  = Block Label [RTL.Inst] [In] [Out]
  deriving (Show)

data CFG
  = CFG String (Map.Map Label BasicBlock)
  deriving (Show)

newtype CFGs = CFGs (Map.Map String CFG)
  deriving (Show)

fromRTL :: RTL.Program -> CFGs
fromRTL (RTL.Program fns entry) = CFGs $ Map.fromList (entryCFGPair : fnCFGPairs)
  where
    entryCFGPair = ("main", instrToCFG "main" entry)
    fnCFGPairs = map processFn fns
    processFn (RTL.Fn name instr) = (name, instrToCFG name instr)

    isNewBlock (RTL.NewBlock {}) = True
    isNewBlock _ = False

    instrToCFG name instr =
      let basicBlocks = instrToBasicBlocks [] [] instr
          mp = Map.fromList $ map (\block@(Block label _ _ _) -> (label, block)) basicBlocks
       in CFG name mp

    instrToBasicBlocks edges blocks [] =
      reverse . flip map blocks $ \(label, instr) ->
        let outs = map snd . filter ((== label) . fst) $ edges
            ins = map fst . filter ((== label) . snd) $ edges
         in Block label instr ins outs
    instrToBasicBlocks edges blocks (RTL.NewBlock label : instr) =
      let (instr', remain) = break isNewBlock instr
          outLabels =
            mapMaybe
              ( \case
                  (RTL.Goto (RTL.Label dst)) -> Just dst
                  (RTL.Jeq _ _ (RTL.Label dst)) -> Just dst
                  (RTL.Move RTL.RLK (RTL.Label dst)) -> Just dst
                  (RTL.Call (RTL.Label "exit") _) -> Just endLabel
                  RTL.Ret -> Just endLabel
                  _ -> Nothing
              )
              instr'
          edges' = map (label,) outLabels
          block = (label, instr')
       in instrToBasicBlocks (edges' ++ edges) (block : blocks) remain
    instrToBasicBlocks _ _ _ = undefined

    startLabel = "start"
    endLabel = "end"

