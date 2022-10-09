{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLists #-}

module Opal.Run.Parse
  ( -- * TODO

    -- * TODO
    pCommand,
    pCmdEval,
    pCmdExpand,
    pCmdParse,
    pCmdRead,
    pKeyword,
  )
where

import Control.Applicative (many, some, (<|>))

import Data.Functor (($>))

import Text.Parsel (Parse)
import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Run.Command (Command (CmdRead, CmdEval, CmdParse, CmdExpand))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pCommand :: Parse Command
pCommand = Parsel.choice [pCmdEval, pCmdExpand, pCmdParse, pCmdRead]

-- | TODO
--
-- @since 1.0.0
pCmdEval :: Parse Command
pCmdEval = do
  pKeyword "eval"
  filepaths <- many pFilePath
  pure (CmdEval filepaths)

-- | TODO
--
-- @since 1.0.0
pCmdExpand :: Parse Command
pCmdExpand = do
  pKeyword "expand"
  filepaths <- many pFilePath
  pure (CmdExpand filepaths)

-- | TODO
--
-- @since 1.0.0
pCmdParse :: Parse Command
pCmdParse = do
  pKeyword "parse"
  filepaths <- many pFilePath
  pure (CmdParse filepaths)

-- | TODO
--
-- @since 1.0.0
pCmdRead :: Parse Command
pCmdRead = do
  pKeyword "read"
  filepaths <- many pFilePath
  pure (CmdRead filepaths)

-- | TODO
--
-- @since 1.0.0
pFilePath :: Parse FilePath
pFilePath = some (Parsel.alphaNum <|> Parsel.char '/' <|> Parsel.char '.')

-- | TODO
--
-- @since 1.0.0
pKeyword :: String -> Parse ()
pKeyword kw = Parsel.string kw *> some Parsel.whitespace $> ()