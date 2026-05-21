{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Jacob Leach, 2026
-- License     :  ISC, see LICENSE
--
-- The @opal@ CLI. Three subcommands:
--
-- * @opal run    FILE@ — expand the file as a module, evaluate each
--   user-level @(define ...)@, print resulting bindings.
-- * @opal expand FILE@ — expand the module and print the resulting
--   syntax form.
-- * @opal parse  FILE@ — expand and parse to 'SExp', print it.
--
-- Global flags:
--
-- * @--module-path DIR@ (repeatable) — prepended to the default
--   @["lib"]@ search path used by @import@.
-- * @-v@\/@--verbose@ — print the expander's final state to stderr.
module Main (main) where

import Control.Monad (when)

import Data.Default (def)

import Opal.Expander
  ( evaluateModuleDefines
  , runExpandFileWith
  , runExpandAndParseFileWith
  )
import Opal.Expander.Config (ExpandConfig (..))

import Options.Applicative

import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

-- | The full parsed CLI invocation.
data Cmd
  = CmdRun    Opts  -- ^ @opal run FILE@
  | CmdExpand Opts  -- ^ @opal expand FILE@
  | CmdParse  Opts  -- ^ @opal parse FILE@
  deriving (Show)

-- | Per-command options.
data Opts = Opts
  { optFilePath   :: FilePath
  , optVerbose    :: Bool
  , optModulePath :: [FilePath]
    -- ^ Extra search-path entries prepended to the default @["lib"]@.
  }
  deriving (Show)

-- | Build an 'ExpandConfig' from CLI options. Prepends user-supplied
-- @--module-path@ entries to the default search path so the default
-- remains usable while extra paths take precedence.
buildConfig :: Opts -> ExpandConfig
buildConfig o =
  def
    { expand_module_search_path =
        optModulePath o ++ expand_module_search_path (def :: ExpandConfig)
    }

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser progInfo
  case cmd of
    CmdRun    o -> doRun    o
    CmdExpand o -> doExpand o
    CmdParse  o -> doParse  o

progInfo :: ParserInfo Cmd
progInfo = info (parser <**> helper)
  ( fullDesc
 <> progDesc "opal — the Opal compiler and runner"
 <> header   "opal — expand, parse, and run Opal source files"
  )

parser :: Parser Cmd
parser = hsubparser
  ( command "run"
      (info (CmdRun <$> opts)
        (progDesc "Expand FILE, evaluate each (define id rhs), and print bindings."))
 <> command "expand"
      (info (CmdExpand <$> opts)
        (progDesc "Expand FILE as a module and print the resulting syntax."))
 <> command "parse"
      (info (CmdParse <$> opts)
        (progDesc "Expand FILE, parse to SExp, and print it."))
  )

opts :: Parser Opts
opts = Opts
  <$> argument str
        ( metavar "FILE"
       <> help    "Opal source file to operate on."
        )
  <*> switch
        ( long  "verbose"
       <> short 'v'
       <> help  "Print the expander's final state to stderr."
        )
  <*> many
        ( strOption
            ( long    "module-path"
           <> metavar "DIR"
           <> help    "Directory to search for imports (repeatable; prepended to the default ['lib'])."
            )
        )

--------------------------------------------------------------------------------

doExpand :: Opts -> IO ()
doExpand o = do
  (stx, st) <- runExpandFileWith (buildConfig o) (optFilePath o)
  when (optVerbose o) (hPutStrLn stderr (show st))
  print stx

doParse :: Opts -> IO ()
doParse o = do
  (sexp, st) <- runExpandAndParseFileWith (buildConfig o) (optFilePath o)
  when (optVerbose o) (hPutStrLn stderr (show st))
  print sexp

doRun :: Opts -> IO ()
doRun o = do
  let config = buildConfig o
  (_stx, st) <- runExpandFileWith config (optFilePath o)
  when (optVerbose o) (hPutStrLn stderr (show st))
  bindings <- evaluateModuleDefines config st
  mapM_ printBinding bindings
  where
    printBinding (sym, val) = putStrLn (show sym ++ " => " ++ show val)
