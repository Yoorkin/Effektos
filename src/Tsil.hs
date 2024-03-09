{-# LANGUAGE DeriveFunctor #-}
module Tsil where

data Tsil a 
  = Nil 
  | Snoc (Tsil a) a
  deriving (Show,Eq,Ord,Functor)

toList :: Tsil a -> [a]
toList Nil = []
toList (xs `Snoc` x) = x:toList xs

fromList :: [a] -> Tsil a
fromList [] = Nil
fromList (x:xs) = fromList xs `Snoc` x


