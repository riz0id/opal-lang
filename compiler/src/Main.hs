module Main (main) where

import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)

import System.Environment (getArgs)
import System.Exit qualified as Exit
import System.IO qualified as IO

import Text.Parsel

--------------------------------------------------------------------------------

import Opal.Read (runRead)
import Opal.Print qualified as Print

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getExecArgs
  print args
  case NonEmpty.head args of
    "read" ->
      for_ (NonEmpty.tail args) \filepath -> do
        source <- IO.withFile filepath IO.ReadMode IO.hGetContents'
        case runRead source of 
          Left exn -> do 
            IO.hPutStrLn IO.stderr ("opal: error reading file: " ++ filepath)
            IO.hPutStrLn IO.stderr (show exn) 
          Right stx -> do 
            IO.hPutStrLn IO.stdout (Print.pprSyntax stx)
    other -> do
      IO.hPutStrLn IO.stderr ("opal: unknown command specified: " ++ other)
      Exit.exitFailure

getExecArgs :: IO (NonEmpty String)
getExecArgs = do
  args <- getArgs
  case NonEmpty.nonEmpty args of
    Just rx -> pure rx
    Nothing -> do
      IO.hPutStrLn IO.stderr "opal: no command specified."
      Exit.exitFailure
