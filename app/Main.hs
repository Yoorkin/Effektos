module Main (main) where

import Lib
import TransToCPS
import qualified CPS as C
import Lambda
import UniqueNameConv

main :: IO ()
main = do 
        visit $ Fix [("id", ("x",(Var "x")))] $ (App (Var "id") (Var "x"))
        pure ()


-- print $ translate $ Fix [("id", ("x",(Var "x")))] $ (App (Var "id") (Var "x"))
