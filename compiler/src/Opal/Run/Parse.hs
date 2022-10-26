{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Run.Parse
  ( -- * TODO

    -- * TODO
    pCommand,
    pCmdExpand,
    pCmdParse,
    pCmdRead,
    pKeyword,
  )
where

import Control.Applicative (many, some, (<|>))

import Data.Functor (($>))
import Data.Text (Text)

import Text.Parsel (Grammar)
import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Run.Command (Command (..))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pCommand :: Grammar Command
pCommand = Parsel.choice [pCmdEval, pCmdExpand, pCmdParse, pCmdRead]

-- | TODO
--
-- @since 1.0.0
pCmdEval :: Grammar Command
pCmdEval = do
  pKeyword "eval"
  filepaths <- many pFilePath
  pure (CmdEval filepaths)

-- | TODO
--
-- @since 1.0.0
pCmdExpand :: Grammar Command
pCmdExpand = do
  pKeyword "expand"
  filepaths <- many pFilePath
  pure (CmdExpand filepaths)

-- | TODO
--
-- @since 1.0.0
pCmdParse :: Grammar Command
pCmdParse = do
  pKeyword "parse"
  filepaths <- many pFilePath
  pure (CmdParse filepaths)

-- | TODO
--
-- @since 1.0.0
pCmdRead :: Grammar Command
pCmdRead = do
  pKeyword "read"
  filepaths <- many pFilePath
  pure (CmdRead filepaths)

-- | TODO
--
-- @since 1.0.0
pFilePath :: Grammar FilePath
pFilePath = some (Parsel.alphaNum <|> Parsel.char '/' <|> Parsel.char '.')

-- | TODO
--
-- @since 1.0.0
pKeyword :: Text -> Grammar ()
pKeyword kw = Parsel.string kw *> some Parsel.whitespace $> ()