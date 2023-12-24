module Main (main) where

import Lib
import TransToCPS
import qualified CPS as C
import Lambda
import UniqueNameConv(transform)

main :: IO ()
main = do 
        print $ transform $ Let "id" (Abs "x" (Var "x")) (App (Var "id") (Var "x"))


-- print $ translate $ Fix [("id", ("x",(Var "x")))] $ (App (Var "id") (Var "x"))
