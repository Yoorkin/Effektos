{-# LANGUAGE LambdaCase #-}

module Sexp (parse, Sexp (..)) where

data Sexp
  = SList [Sexp]
  | SAtom String

instance Show Sexp where
    show (SAtom s) = s
    show (SList xs) = "(" ++ unwords (map show xs) ++ ")"

parse :: String -> Sexp
parse = fst . parseList

parseList :: String -> (Sexp, String)
parseList ('(' : rest) = f [] rest
  where
    f acc = \case
      (')' : xs) -> (SList $ reverse acc, xs)
      xs@('(' : _) -> let (s, r) = parseList xs in f (s : acc) r
      (x : xs) | x `elem` "\n\t " -> f acc xs
      xs -> let (s, xs') = span (`notElem` "()\n\t ") xs in f (SAtom s : acc) xs'
parseList e = error $ "invalid input:" ++ e

-- data PrettyLayout 
--   = Group [PrettyLayout]
--   | Item Sexp


