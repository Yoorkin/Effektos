module ASM.RegAlloc where

import ASM.Graph
import ASM.Liveness
import ASM.RTL
import Data.Map as Map (Map, elems, (!), empty, insert)
import Data.List ( find, (\\) )

type Register = Operand

type Allocation = Map Operand Register

getRenderList :: Eq a => Int -> Graph a -> [a] -> [a]
getRenderList maxReg graph acc
  | let vexs = allVertix graph, not (null vexs) =
    case find ((<=maxReg) . length . flip edgesOf graph) vexs of
      Nothing -> error "alloc failed"
      Just vex -> getRenderList maxReg (removeVertix vex graph) (vex:acc)
  | otherwise = reverse acc

allocation :: [Register] -> FnLiveInfo -> Allocation
allocation availableReg (FnLiveInfo blocks) = render Map.empty renderList
  where
    conflictsList = concatMap (\(a, b) -> [a, b]) $ concat $ Map.elems blocks

    conflictEdges conflicts = [(x, y) | x <- conflicts, y <- conflicts, x /= y]

    graph = fromEdges (concatMap conflictEdges conflictsList)

    maxReg = length availableReg

    renderList = getRenderList maxReg graph []

    render colorMap [] = colorMap
    render colorMap (x:xs) =
      let available = availableReg \\ map (colorMap !) (inVertix x graph ++ outVertix x graph)
          reg = head available
       in render (Map.insert x reg colorMap) xs






