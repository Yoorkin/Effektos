{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Graph where

data Graph a
  = Graph [Edge a]

type Edge a = (a,a)

removeVertix :: Eq a => a -> Graph a -> Graph a
removeVertix v (Graph edges) = Graph (filter notEdgeOfV edges)
    where 
      notEdgeOfV (a,b) 
              | a /= v && b /= v = False
              | otherwise = True

inVertix, outVertix :: Eq a => Graph a -> a -> [a]
inVertix (Graph edges) v = map fst $ filter ((==) v . snd) edges
outVertix (Graph edges) v = map snd $ filter ((==) v . fst) edges

fromEdges :: Eq a => [Edge a] -> Graph a
fromEdges = Graph


