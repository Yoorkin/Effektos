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
import System.Console.CmdArgs

data CommandOptions
  = CompileMode String 
  | TestMode String
  deriving Show

compileMode = CompileMode (def &= args)

testMode = TestMode (def &= args)

  
main :: IO ()
main = print =<< cmdArgs (modes [compileMode,testMode])


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



