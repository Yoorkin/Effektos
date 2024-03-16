{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ASM.Graph where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Prettyprinter
import Util.Prettyprint

data Graph a
  = Graph [a] [Edge a]
  deriving (Show)

newtype Edge a = Edge (a, a)

instance (Eq a) => Eq (Edge a) where
  Edge (a1, b1) == Edge (a2, b2)
    | a1 == a2 && b1 == b2 = True
    | a1 == b2 && b1 == a2 = True
    | otherwise = False

instance (Show a) => Show (Edge a) where
  show (Edge a) = show a

isEdgeOf :: (Eq a) => a -> Edge a -> Bool
isEdgeOf v (Edge (a, b))
  | a == v || b == v = True
  | otherwise = False

edgesOf :: (Eq a) => a -> Graph a -> [Edge a]
edgesOf v (Graph _ edges) = filter (isEdgeOf v) edges

removeVertix :: (Eq a, Show a) => a -> Graph a -> Graph a
removeVertix v (Graph vex edges) =
    Graph (filter (/= v) vex) (filter (not . isEdgeOf v) edges)

inOutVexs :: (Eq a) => a -> Graph a -> [a]
inOutVexs v (Graph _ edges) = mapMaybe f edges
  where
    f (Edge (a, b))
      | a == v = Just b
      | b == v = Just a
      | otherwise = Nothing

allVertix :: (Eq a) => Graph a -> [a]
allVertix (Graph vexs _) = vexs

fromVertixEdges :: (Eq a) => [a] -> [Edge a] -> Graph a
fromVertixEdges = Graph
