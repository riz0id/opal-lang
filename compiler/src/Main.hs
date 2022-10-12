module Main (main) where

import Control.Exception (ErrorCall (ErrorCall), throwIO)

import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.IO qualified as Text.IO
import Data.Text (Text)

import System.Environment (getArgs)
import System.Exit qualified as Exit
import System.IO qualified as IO

import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Core (Expr, Datum)

import Opal.Expand.Syntax (Syntax)

import Opal.Print qualified as Print

import Opal.Read qualified as Read

import Opal.Parse qualified as Parse
import Opal.Run.Command (Command (CmdEval, CmdParse, CmdRead, CmdExpand))
import Opal.Run.Parse qualified as Parse
import qualified Opal.Evaluate as Eval
import qualified Opal.Expand as Expand
import qualified Data.Text as Text

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- getExecCommand
  case cmd of
    CmdEval filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        value <- evalOpalIO source
        IO.hPutStrLn IO.stdout (filepath ++ ": evaluated expression: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprDatum value)
    CmdExpand filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        stx <- expandOpalIO source
        IO.hPutStrLn IO.stdout (filepath ++ ": expanded expression: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprSyntax stx)
    CmdParse filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        sexp <- parseOpalIO source
        IO.hPutStrLn IO.stdout (filepath ++ ": parsed expression: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprSExp sexp)
    CmdRead filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        syntax <- readOpalIO source
        IO.hPutStrLn IO.stdout (filepath ++ ": read syntax: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprSyntax syntax)

evalOpalIO :: Text -> IO Datum
evalOpalIO source = do 
  sexp <- parseOpalIO source 
  case Eval.runKernalEval sexp of 
    Left exn -> throwIO (ErrorCall $ show exn)
    Right val -> pure val

expandOpalIO :: Text -> IO Syntax
expandOpalIO source = do 
  stx <- readOpalIO source 
  case Expand.runExpandSyntax stx of 
    Left exn -> throwIO (ErrorCall $ show exn)
    Right stx' -> pure stx'

parseOpalIO :: Text -> IO Expr
parseOpalIO source = do
  syntax <- readOpalIO source
  case Parse.evalParseExpr syntax of
    Left exn -> throwIO (ErrorCall $ show exn)
    Right sexp -> pure sexp

readOpalIO :: Text -> IO Syntax
readOpalIO source =
  case Read.runRead source of
    Left exn -> throwIO (ErrorCall $ show exn)
    Right stx -> pure stx

getOpalFileIO :: FilePath -> IO Text
getOpalFileIO filepath = IO.withFile filepath IO.ReadMode Text.IO.hGetContents

getExecCommand :: IO Command
getExecCommand = do
  args <- getArgs
  case NonEmpty.nonEmpty args of
    Just {} ->
      let input :: Text 
          input = Text.unwords (map Text.pack args)
       in case Parsel.parse input Parse.pCommand of
            Left exn -> throwIO (ErrorCall $ show exn)
            Right cmd -> pure cmd
    Nothing -> do
      IO.hPutStrLn IO.stderr "opal: no command specified."
      Exit.exitFailure
