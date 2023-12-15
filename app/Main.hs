module Main (main) where

import Lib
import TransToCPS
import qualified CPS as C
import Lambda

main :: IO ()
main = print $ translate $ Let "id" (Abs "x" (Var "x")) $ (App (Var "id") (Var "x"))
