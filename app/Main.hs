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
import Flatvm
import Parser(parse)
import Lexer(tokenize,Token(..))

toCPS = translate . uniquify 

z = parse . tokenize $ "let compute = fun x -> 5-1+x in let dat = (1,2,3,4) in compute (compute (get2 dat))"

z1 =  
   "(let i 2 (let f (lambda x (+ x i)) (f (f (f 3)))))"

z2 = parse . tokenize $ "let f = fun x -> x in f 3"

eff = parse . tokenize $ 
        "let rec consoleHandler = fun e -> \
        \   handle e with \
        \   | Print(x,k) -> k () \
        \   | Input(x,k) -> k 5 \
        \in \
        \let rec loop = fun n -> if n > 0 then (Print n; loop (n-1)) else () in \
        \consoleHandler (loop 5)"     

eff1 = 
        "let rec consoleHandler = fun e -> \
        \   handle e with \
        \   | Print(x,k) -> output x; consoleHandler (k ()) \
        \   | Input(x,k) -> consoleHandler (k ()) \
        \in \
        \consoleHandler (loop 5)"     

main :: IO ()
main = do 
        pPrint $ eff1
-- print $ translate $ Fix [("id", ("x",(Var "x")))] $ (App (Var "id") (Var "x"))
