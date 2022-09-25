{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Control.Exception (Exception (displayException), SomeException (SomeException), try)

import Control.Monad (unless, (<=<), (>=>), when)
import Control.Monad.IO.Class (liftIO)

import Data.Function (fix)
import Data.Functor (void)
import Data.IO.FileBuffer (FileBuffer)
import Data.IO.FileBuffer qualified as FileBuffer
import Data.IORef (modifyIORef', newIORef, readIORef)

import Opal.Expand.Eval qualified as Eval
import Opal.Expand qualified as Expand
import Opal.Expand.Syntax (Syntax)

import Prelude hiding (exp)

import Prettyprinter qualified as Print

import System.Exit qualified as Exit
import System.IO (Handle)
import System.IO qualified as IO

import Text.Printf qualified as Text

--------------------------------------------------------------------------------

import Control.Monad.Cont (ContM, callCC, runContM, shiftM)

import Opal.Expand (evalExpand, runStxExpand, expEvaluate)
import Opal.Expr (Datum, Expr)
import Opal.Lexer qualified as Lexer
import Opal.Print qualified as Print
import Opal.Repl.CLI (CommandCLI, InputCLI, ParseError)
import Opal.Repl.CLI qualified as CLI
import Opal.Repl.Parse (parseCLI)
import Opal.Repl.Input

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Opal REPL v0.0.0"
  putStrLn "* Type :? for help"
  putStrLn "* Type :q to close the repl"

  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetEcho IO.stdin False

  repl

promptREPL :: IO ()
promptREPL = do 
  IO.putStr "λ= "
  IO.hFlush IO.stdout

repl :: IO ()
repl = do
  -- promptREPL
  input <- inputIO 
  putStrLn input
  putStrLn ""
  repl
  -- case parseCLI input of
  --   Left err -> handleParseErrorCLI err
  --   Right cli -> dispatchCLI cli

handleParseErrorCLI :: ParseError -> IO ()
handleParseErrorCLI exn =
  unless (exn == CLI.NoInputError) do
    putStrLn "Lexical error:"
    print exn
    repl

dispatchCLI :: InputCLI -> IO ()
dispatchCLI (CLI.EvalCLI stx) = evalCLI stx
dispatchCLI (CLI.CmdCLI cmd) = cmdCLI cmd

evalCLI :: Syntax -> IO ()
evalCLI stx = do
  case Expand.evalExpand (Expand.runStxExpand stx) of
    Left err -> do
      putStrLn "Parse error:"
      print err
    Right exp -> do
      case evalExpand (expEvaluate exp) of
        Left err -> do
          putStrLn "Evaluation error:"
          print err
        Right rx -> do
          putStrLn "Evaluation result:"
          print rx
  repl

cmdCLI :: CommandCLI -> IO ()
cmdCLI CLI.ExpandCmd = do
  runContM runExpanderCLI \expr -> 
    putStrLn (Print.render $ Print.pretty expr)
cmdCLI CLI.LexCmd = do
  runContM runLexerCLI \stx -> 
    putStrLn (Print.render $ Print.pprSyntax stx) >> repl
cmdCLI CLI.HelpCmd = do
  putStrLn "The :? command is not implemented" >> repl
cmdCLI CLI.LoadCmd = do
  runContM runEvaluatorCLI \val -> 
    putStrLn (Print.render $ Print.pretty val) >> repl
cmdCLI CLI.ReloadCmd = do
  putStrLn "The :r command is not implemented" >> repl
cmdCLI CLI.QuitCmd = do
  putStrLn "Leaving Opal REPL."

runEvaluatorCLI :: ContM () IO Datum
runEvaluatorCLI = do
  stx <- runLexerCLI 
  shiftM \next -> 
    liftIO case evalExpand (expEvaluate =<< runStxExpand stx) of
      Left exn -> putStrLn ("expander error: " ++ show exn)
      Right (_, rx) -> next rx

runExpanderCLI :: ContM () IO Expr
runExpanderCLI = do 
  stx <- runLexerCLI 
  shiftM \next -> 
    liftIO case evalExpand (runStxExpand stx) of
      Left exn -> putStrLn ("expander error: " ++ show exn)
      Right rx -> next rx

runLexerCLI :: ContM () IO Syntax
runLexerCLI = do 
  buffer <- tryLoadFileCLI =<< liftIO inputIO 
  shiftM \next -> 
    liftIO case Lexer.runLexer buffer Lexer.lexStx of
      Left exn -> putStrLn ("lexer error: " ++ show exn)
      Right stx -> next stx

tryLoadFileCLI :: FilePath -> ContM () IO FileBuffer
tryLoadFileCLI filepath = do 
  result <- liftIO (try (FileBuffer.openFileBufferIO filepath))
  shiftM \next -> 
    liftIO case result of 
      Left exn -> putStrLn (displayException @SomeException exn)
      Right buf -> next buf

