module Main (main) where

import Control.Exception (ErrorCall (ErrorCall), throwIO)

import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import Prelude hiding (exp)

import System.Environment (getArgs)
import System.Exit qualified as Exit
import System.IO qualified as IO

import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Core (Expr)

import Opal.Expand.Syntax (Syntax)

import Opal.Print qualified as Print

import Opal.Read qualified as Read

import Opal.Expand qualified as Expand
import Opal.Parse qualified as Parse
import Opal.Run.Command (Command (..))
import Opal.Run.Parse qualified as Parse
import Opal.Core.Datum (Datum)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- getExecCommand
  case cmd of
    CmdEval filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        expr <- evalOpalIO source
        putStrLn (filepath ++ ": expanded+evaluated expression: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprDatum expr)
    CmdExpand filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        stxs <- expandOpalIO source
        putStrLn (filepath ++ ": expanded expression: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprSyntax stxs)
    CmdParse filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        expr <- parseOpalIO source
        putStrLn (filepath ++ ": parsed expression: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprExpr expr)
    CmdRead filepaths ->
      for_ filepaths \filepath -> do
        source <- getOpalFileIO filepath
        stx <- readOpalIO source
        putStrLn (filepath ++ ": read syntax: ")
        Text.IO.hPutStrLn IO.stdout (Print.pprSyntax stx)

evalOpalIO :: Text -> IO Datum
evalOpalIO source = do
  stx <- readOpalIO source
  case Expand.runSyntaxEval stx of
    Left exn -> throwIO (ErrorCall $ show exn)
    Right stx' -> pure stx'

expandOpalIO :: Text -> IO Syntax
expandOpalIO source = do
  stx <- readOpalIO source
  case Expand.runSyntaxExpand stx of
    Left exn -> throwIO (ErrorCall $ show exn)
    Right stx' -> pure stx'

parseOpalIO :: Text -> IO Expr
parseOpalIO source = do
  syntax <- readOpalIO source
  case Parse.evalParseExpr syntax of
    Left exn -> throwIO (ErrorCall $ show exn)
    Right decls -> pure decls

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
