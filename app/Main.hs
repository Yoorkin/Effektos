module Main (main) where

import Lib
import TransToCPS
import qualified CPS as C
import Lambda
import Uniquify(uniquify)
import Text.Pretty.Simple(pPrint)
import Parser(parse)
import qualified DCE
import qualified BetaReduction

toCPS = translate . uniquify 

x = toCPS $ Let "id" (Abs "x" (Var "x")) (Let "x" (Const $ Integer 10) (Var "x"))
sel = toCPS $ Let "xi" (Select 1 (Tuple [Const (Integer 1),Const (Integer 2)])) (Var "xi")







z = parse 
   "(let compute (lambda x (+ (- 5 1) x)) (let data (tuple 1 2 3 4) (compute (compute (select 2 data)))))"

z1 =  
   parse "(let f (lambda x (+ x 1)) (f (f (f 3))))"

z2 = parse 
   "(let f (lambda x x) (f 3))"

main :: IO ()
main = do 
        pPrint $ x
-- print $ translate $ Fix [("id", ("x",(Var "x")))] $ (App (Var "id") (Var "x"))
