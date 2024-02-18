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

test2 =
  "let f = fun x -> x + 5 in f (f 114)"

closureMap = 
  "let empty = fun key -> () in \
  \let append = fun k -> fun v -> fun m -> \
  \             (fun x -> if x == k then v else m x) in \
  \let table = append 1 2 empty in \
  \table 4"

iterator =
  "let print = fun x -> (extern \"console.log\" x) in \
  \let rec iter = fun x -> let n = raise (Yield,x) in print n; iter n in \
  \handle iter 1 with \
  \| Yield (x,k) -> if x > 5 then 114514 else resume (k,x + 2) "


iterator2 =
  "let print = fun x -> (extern \"console.log\" x) in \
  \let rec iterator = \
  \    fun l -> iterator (raise (Yield, l)) in \
  \handle iterator 1 with \
  \| Yield (x,k) -> print x; if x > 10 then 114514 else resume (k,x + 2) "

printTest = 
  "let print = fun x -> (extern \"console.log\" x) in \
  \print 114514"

stateTest = "let print = fun x -> (extern \"console.log\" x) in \n\
\let runState = \n\
\  fun program -> fun initial -> \n\
\    let s = \n\
\      handle program () with\n\
\      | Put (x,k) -> fun ignored -> (resume (k,())) x \n\
\      | Get (ignored,k) -> fun y -> (resume (k,y)) y \n\
\    in s initial \n\
\in \n\
\runState (fun ignored -> \n\
\  let x = raise (Get, ()) in \n\
\  print x; \n\
\  raise (Put, x + 10); \n\
\  let x2 = raise (Get, ()) in \n\
\  print x2  \n\
\) 5 "

compile :: String -> CompEnvT IO Flat.Program
compile input = do
      lift $ putStrLn "=========== Source ================"
      lift $ putStrLn input
      lift $ putStrLn "=========== Tokens ================"
      let tokens = tokenize input 
      lift $ pPrint tokens
      lift $ putStrLn "=========== Lambda ================"
      let syntax = parse tokens
      lambda <- hoistIO (SyntaxToLambda.transProg syntax)
      lift $ pPrint lambda
      -- lift $ print lambda
      lift $ putStrLn "=========== Uniquified ================"
      lambda <- hoistIO (Uniquify.uniquifyTerm lambda)
      lift $ print lambda
      -- lift $ print cps
      lift $ putStrLn "=========== CPS ================"
      cps <- hoistIO (TransToCPS.translate lambda)
      lift $ print cps
      lift $ putStrLn "=========== Simplified CPS ================"
      -- cps <- hoistIO (Simp.simplify cps)
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

