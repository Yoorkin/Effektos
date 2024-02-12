{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import qualified Simp
import qualified ClosureConversion
import CompileEnv
import Control.Monad.State.Lazy
import qualified Flat
import qualified HoistToFlat
import Lexer (tokenize)
import Parser (parse)
import qualified SyntaxToLambda
import Text.Pretty.Simple (pPrint)
import TransToCPS
import Uniquify (uniquifyTerm)
import Control.Monad.Morph (hoist, generalize)
import qualified Lambda
import Control.Comonad.Identity (runIdentity)
import CompileEnv (hoistIO)
import TransToJS

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
  "let rec f = fun x -> g (x + 1) \
  \and g = fun x -> if 4 > x then f (x + 1 + 1) else x in \
  \f 1"

test1 =
  "let f = fun x -> x + 1 in\
    \ f 114"

compile :: String -> CompEnvT IO Flat.Program
compile input = do
      let syntax = parse . tokenize $ input
      lambda <- hoistIO (SyntaxToLambda.transProg syntax)
      lift $ putStrLn "=========== Lambda ================"
      lift $ print lambda
      cps <- hoistIO (TransToCPS.translate lambda)
      lambda <- hoistIO (Uniquify.uniquifyTerm lambda)
      -- lift $ print lambda
      lift $ putStrLn "=========== Uniquified ================"
      lift $ print lambda
      cps <- hoistIO (TransToCPS.translate lambda)
      -- lift $ print cps
      cps <- hoistIO (Simp.simplify cps)
      lift $ putStrLn "=========== CPS ================"
      lift $ print cps
      clo <- hoistIO (ClosureConversion.translClosure cps)
      lift $ putStrLn "=========== Closure Passing Style ================"
      lift $ print clo
      flat <- hoistIO (HoistToFlat.hoistToFlat clo)
      lift $ putStrLn "=========== Flat ================"
      lift $ print flat
      let js = TransToJS.transl flat
      lift $ putStrLn "=========== JS ================"
      lift $ putStrLn js
      pure flat


main :: IO ()
main = do
  pPrint eff1

