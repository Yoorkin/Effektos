module Main (main) where

import qualified BetaReduction
import qualified ClosureConversion
import qualified HoistToFlat
import Lexer (tokenize)
import Parser (parse)
import qualified SyntaxToLambda
import Text.Pretty.Simple (pPrint)
import TransToCPS
import Uniquify (uniquifyTerm)
import qualified Flat

toCPS = translate . uniquifyTerm

z = parse . tokenize $ "let compute = fun x -> 5-1+x in let dat = (1,2,3,4) in compute (compute (get2 dat))"

z1 =
  "(let i 2 (let f (lambda x (+ x i)) (f (f (f 3)))))"

z2 = parse . tokenize $ "let f = fun x -> x in f 3"

eff =
    "let rec consoleHandler = fun e -> \
    \   handle e with \
    \   | Print(x,k) -> k () \
    \   | Input(x,k) -> let plus1 = fun x -> x + 1 in k (plus1(plus1(5))) \
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


mutrec = 
  "let rec f = fun x -> g (x - 1) \
  \and g = fun x -> if x > 10 then f (x - 2) else x in \
  \f 114"

compToFlat :: String -> Flat.Program
compToFlat =
  HoistToFlat.hoistToFlat
    . ClosureConversion.transClosure 100
    . BetaReduction.simplify
    . TransToCPS.translate
    . Uniquify.uniquifyTerm
    . SyntaxToLambda.transProg
    . parse
    . tokenize


compToClosurePassingStyle =
    ClosureConversion.transClosure 100
    . BetaReduction.simplify
    . TransToCPS.translate
    . Uniquify.uniquifyTerm
    . SyntaxToLambda.transProg
    . parse
    . tokenize

compToSimplifiedCPS = 
    BetaReduction.simplify
    . TransToCPS.translate
    . Uniquify.uniquifyTerm
    . SyntaxToLambda.transProg
    . parse
    . tokenize

compToCPS = 
    TransToCPS.translate
    . Uniquify.uniquifyTerm
    . SyntaxToLambda.transProg
    . parse
    . tokenize

main :: IO ()
main = do
  pPrint $ eff1

-- print $ translate $ Fix [("id", ("x",(Var "x")))] $ (App (Var "id") (Var "x"))
