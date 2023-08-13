
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Reader.Monad
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Reader.Monad
  ( -- * Reader
    Reader (..)
    -- ** Query Operations
  , readerFilePath
  , readerSyntaxInfo
  , readerSrcLoc
    -- * ReaderError
  , ReaderError (..)
  )
where

import Control.Applicative (Alternative (..))

import Control.Monad (MonadPlus)

import Data.Default (Default (..))
import Data.Text (Text)

import Opal.Common.SourceInfo (SourceInfo (..))
import Opal.Common.SrcLoc (SrcLoc (..))
import Opal.Syntax (SyntaxInfo (..))

import Text.Megaparsec
  ( MonadParsec
  , Parsec
  , ShowErrorComponent(..)
  , getOffset
  , getSourcePos
  , sourceColumn
  , sourceLine
  , sourceName
  , unPos
  )

-- Reader ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Reader a = Reader
  { unReader :: Parsec ReaderError Text a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadParsec ReaderError Text
    )

-- Reader - Query Operations ---------------------------------------------------

-- | Obtain the the Reader's current lexical information as a 'SyntaxInfo'.
--
-- @since 1.0.0
readerSyntaxInfo :: Reader SyntaxInfo
readerSyntaxInfo = do
  srcInfo <- readerSourceInfo
  pure def { stx_info_source = Just srcInfo }

-- | Obtain the the Reader's current source information as a 'SourceInfo'.
--
-- @since 1.0.0
readerSourceInfo :: Reader SourceInfo
readerSourceInfo = liftA2 SourceInfo readerFilePath readerSrcLoc

-- | Obtain the source 'FilePath' the reader is currently processing.
--
-- @since 1.0.0
readerFilePath :: Reader FilePath
readerFilePath = do
  sourcePos <- getSourcePos
  pure (sourceName sourcePos)

-- | Obtain the position within the source file that is being read from as a
-- source location.
--
-- @since 1.0.0
readerSrcLoc :: Reader SrcLoc
readerSrcLoc = do
  posn   <- getOffset
  srcPos <- getSourcePos
  let line = unPos (sourceLine srcPos)
  let coln = unPos (sourceColumn srcPos)
  pure (SrcLoc posn line coln)

-- ReaderError -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ReaderError
  = ReaderError String
  deriving (Eq, Ord, Show)

-- | @since 1.0.0
instance ShowErrorComponent ReaderError where
  showErrorComponent (ReaderError msg) = msg
