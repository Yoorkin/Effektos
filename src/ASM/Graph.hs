{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ASM.Graph where

import Data.List (nub)

data Graph a
  = Graph [Edge a]

type Edge a = (a, a)

edgesOf :: (Eq a) => a -> Graph a -> [Edge a]
edgesOf v (Graph edges) = filter isEdgeOfV edges
  where
    isEdgeOfV (a, b)
      | a == v && b == v = True
      | otherwise = False

removeVertix :: (Eq a) => a -> Graph a -> Graph a
removeVertix v (Graph edges) = Graph (filter notEdgeOfV edges)
  where
    notEdgeOfV (a, b)
      | a /= v && b /= v = False
      | otherwise = True

inVertix, outVertix :: (Eq a) => a -> Graph a -> [a]
inVertix v (Graph edges) = map fst $ filter ((==) v . snd) edges
outVertix v (Graph edges) = map snd $ filter ((==) v . fst) edges

inDegree, outDegree :: (Eq a) => Graph a -> a -> Int
inDegree graph v = length (inVertix v graph)
outDegree graph v = length (outVertix v graph)

allVertix :: (Eq a) => Graph a -> [a]
allVertix (Graph edges) = let (a, b) = unzip edges in nub (a ++ b)

fromEdges :: (Eq a) => [Edge a] -> Graph a
fromEdges = Graph
