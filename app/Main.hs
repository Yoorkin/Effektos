{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import qualified Core.ClosureConversion as ClosureConversion
import Util.CompileEnv
import Control.Monad (when)
import Control.Monad.State.Lazy
import Syntax.Lexer (tokenize,showTokens)
import Syntax.Parser (parse)
import qualified Core.Simp as Simp
import qualified Syntax.SyntaxToLambda as SyntaxToLambda
import System.Console.CmdArgs
import Text.Pretty.Simple (pPrint)
import Lambda.LambdaToCPS as LambdaToCPS
import JS.FlatToJS as FlatToJS
import Lambda.Uniquify as Uniquify
import qualified Core.CPSPrinter as CPSPrinter
import Core.CPS (hoisting)
import qualified Core.CPSToCMM as CPSToCMM
import qualified Core.CPSToRTL as CPSToRTL
import qualified Core.CPSToFlat as CPSToFlat

data Effektos = Compile
  { files :: [FilePath],
    optimize :: Bool,
    show_input :: Bool,
    debug_tokens :: Bool,
    debug_syntax :: Bool,
    debug_lambda :: Bool,
    debug_uniquify :: Bool,
    debug_cps :: Bool,
    debug_simplify :: Bool,
    debug_closure_conversion :: Bool,
    debug_flat :: Bool,
    debug_js :: Bool,
    debug_cmm :: Bool,
    debug_rtl :: Bool
  }
  deriving (Show, Data, Typeable)

type CommandOptions = Effektos

compileMode :: Effektos
compileMode =
  Compile
    { files = def &= args &= typ "FILES/DIRS",
      optimize = def &= help "enable optimization",
      show_input = def,
      debug_tokens = def,
      debug_syntax = def,
      debug_lambda = def,
      debug_uniquify = def,
      debug_cps = def,
      debug_simplify = def,
      debug_closure_conversion = def,
      debug_flat = def,
      debug_rtl = def,
      debug_cmm = def,
      debug_js = def
    }

main :: IO ()
main = do
  options <- cmdArgs (modes [compileMode])
  evalStateT (pipeline options) mkCompStates

pipeline :: CommandOptions -> CompEnvT IO ()
pipeline options = do
  input <- lift $ readFile (head . files $ options)
  when (show_input options) $ do
    lift $ putStrLn "=========== Source ================"
    lift $ putStrLn input
  let tokens = tokenize input
  when (debug_tokens options) $ do
    lift $ putStrLn "=========== Tokens ================"
    lift $ putStrLn (showTokens tokens)
  let syntax = parse tokens
  when (debug_syntax options) $ do
    lift $ putStrLn "=========== Syntax ================"
    lift $ pPrint syntax
  lambda <- hoistIO (SyntaxToLambda.transProg syntax)
  when (debug_lambda options) $ do
    lift $ putStrLn "=========== Lambda ================"
    lift $ print lambda
  lambda <- hoistIO (Uniquify.uniquifyTerm lambda)
  when (debug_uniquify options) $ do
    lift $ putStrLn "=========== Uniquified ================"
    lift $ print lambda
  cps <- hoistIO (LambdaToCPS.translate lambda)
  when (debug_cps options) $ do
    lift $ putStrLn "=========== CPS ================"
    lift $ putStrLn (CPSPrinter.prettyCPS cps)
  cps <-
    if optimize options
      then do
        r <- hoistIO (Simp.simplify cps)
        when (debug_simplify options) $ do
          lift $ putStrLn "=========== Simplified CPS ================"
          lift $ putStrLn (CPSPrinter.prettyCPS r)
        pure r
      else pure cps
  clo <- hoistIO (ClosureConversion.translClosure cps)
  when (debug_closure_conversion options) $ do
    lift $ putStrLn "=========== Closure Passing Style ================"
    lift $ putStrLn (CPSPrinter.prettyCPS clo)
  let hoisted = hoisting clo
  lift $ putStrLn "=========== Hosting ================"
  lift $ print clo
  lift $ putStrLn (CPSPrinter.prettyCPS hoisted)
  cmm <- hoistIO (CPSToCMM.translate hoisted)
  when (debug_cmm options) $ do
    lift $ putStrLn "=========== CMM ================"
    lift $ putStrLn (show cmm)
  flat <- hoistIO (CPSToFlat.hoistToFlat clo)
  when (debug_flat options) $ do
    lift $ putStrLn "=========== Flat ================"
    lift $ print flat
  let js = FlatToJS.transl flat
  when (debug_js options) $ do
    lift $ putStrLn "=========== JS ================"
    lift $ putStrLn js
  let rtl = CPSToRTL.translate hoisted 
  when (debug_rtl options) $ do
    lift $ putStrLn "=========== RTL ================"
    lift $ print rtl

