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
import Util (free, bound, var, occur)

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
  "let y = 2 in let rec f = fun x -> g (x + y) \
  \and g = fun x -> if 4 > x then f (x + y) else x in \
  \f 1"

test1 =
  "let n = 5 + 2 in let f = fun x -> x + n in f (f 114)"

compile :: String -> CompEnvT IO Flat.Program
compile input = do
      let syntax = parse . tokenize $ input
      lift $ putStrLn "=========== Lambda ================"
      lambda <- hoistIO (SyntaxToLambda.transProg syntax)
      lift $ print lambda
      -- lift $ print lambda
      lift $ putStrLn "=========== Uniquified ================"
      lambda <- hoistIO (Uniquify.uniquifyTerm lambda)
      lift $ print lambda
      -- lift $ print cps
      -- cps <- hoistIO (Simp.simplify cps)
      lift $ putStrLn "=========== CPS ================"
      cps <- hoistIO (TransToCPS.translate lambda)
      lift $ print cps
      lift $ putStrLn "=========== Closure Passing Style ================"
      clo <- hoistIO (ClosureConversion.translClosure cps)
      lift $ print clo
      lift $ putStrLn "=========== Flat ================"
      flat <- hoistIO (HoistToFlat.hoistToFlat clo)
      lift $ print flat
      lift $ putStrLn "=========== JS ================"
      let js = TransToJS.transl flat
      lift $ putStrLn js
      pure flat


main :: IO ()
main = do
  pPrint eff1

