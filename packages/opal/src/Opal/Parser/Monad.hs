{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Parser.Monad
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Parse' monad along with its associated read-only
-- state 'ParseConfig'.
--
-- @since 1.0.0
module Opal.Parser.Monad
  ( -- * Parse
    Parse (..)
    -- ** Basic Operations
  , runParse
    -- ** Error Operations
  , throwEmptyAppParseError
  , throwLambdaParseError
  , throwQuoteParseError
  , throwQuoteSyntaxParseError
    -- * ParseConfig
  , ParseConfig (..)
    -- ** Lenses
  , parseBindingStore
  , parseCurrentPhase
    -- * ParseError
  , ParseError (..)
    -- * CoreParseError
  , CoreParseError (..)
  )
where

import Control.Lens (Lens', lens)

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))

import Data.Default (Default (..))
import Data.Function ((&))

import GHC.Generics (Generic)

import Opal.Common.BindingStore (BindingStore)
import Opal.Common.Phase (Phase)
import Opal.Syntax

-- Parse -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Parse a = Parse
  { unParse :: ReaderT ParseConfig (ExceptT ParseError IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ParseError
    , MonadReader ParseConfig
    )

-- Parse - Basic Operations ----------------------------------------------------

-- | Run an 'Parse' computation with the given 'ParseConfig'.
--
-- @since 1.0.0
runParse :: ParseConfig -> Parse a -> IO (Either ParseError a)
runParse c parse =
  unParse parse
    & flip runReaderT c
    & runExceptT

-- Parse - Error Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
throwEmptyAppParseError :: SyntaxInfo -> Parse a
throwEmptyAppParseError = throwError . ParseErrorEmptyApp

-- | TODO: docs
--
-- @since 1.0.0
throwLambdaParseError :: Syntax -> Parse a
throwLambdaParseError = throwError . ParseErrorCore . ParseErrorLambda

-- | TODO: docs
--
-- @since 1.0.0
throwQuoteParseError :: Syntax -> Parse a
throwQuoteParseError = throwError . ParseErrorCore . ParseErrorQuote

-- | TODO: docs
--
-- @since 1.0.0
throwQuoteSyntaxParseError :: Syntax -> Parse a
throwQuoteSyntaxParseError = throwError . ParseErrorCore . ParseErrorQuoteSyntax

-- ParseConfig -----------------------------------------------------------------

-- | 'ParseConfig' is the read-only state of the 'Parse' monad.
--
-- @since 1.0.0
data ParseConfig = ParseConfig
  { parse_binding_store :: BindingStore
    -- ^ A binding store that is threaded through parsing to substitute
    -- identifiers with the generated symbols they are bound to.
  , parse_current_phase :: {-# UNPACK #-} !Phase
    -- ^ The current phase that 'Parse' is parsing at.
  }
  deriving (Generic, Show)

-- | @since 1.0.0
instance Default ParseConfig where
  def = ParseConfig def def

-- ParseConfig - Lenses --------------------------------------------------------

-- | Lens focusing on the 'parse_binding_store' field of 'ParseConfig'.
--
-- @since 1.0.0
parseBindingStore :: Lens' ParseConfig BindingStore
parseBindingStore = lens parse_binding_store \s x -> s { parse_binding_store = x }

-- | Lens focusing on the 'parse_current_phase' field of 'ParseConfig'.
--
-- @since 1.0.0
parseCurrentPhase :: Lens' ParseConfig Phase
parseCurrentPhase = lens parse_current_phase \s x -> s { parse_current_phase = x }

-- ParseError ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ParseError
  = ParseErrorEmptyApp {-# UNPACK #-} !SyntaxInfo
    -- ^ TODO: docs
  | ParseErrorCore CoreParseError
    -- ^ TODO: docs
  deriving (Show)

-- CoreParseError --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data CoreParseError
  = ParseErrorLambda      {-# UNPACK #-} !Syntax
    -- ^ TODO: docs
  | ParseErrorQuote       {-# UNPACK #-} !Syntax
    -- ^ TODO: docs
  | ParseErrorQuoteSyntax {-# UNPACK #-} !Syntax
    -- ^ TODO: docs
  deriving (Show)