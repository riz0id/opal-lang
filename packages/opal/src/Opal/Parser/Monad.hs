{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

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
    -- * ParseConfig
  , ParseConfig (..)
    -- ** Lenses
  , parseBindingStore
  , parseCurrentPhase
    -- * ParseError
  , ParseError (..)
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))

import Data.Default (Default (..))
import Data.Function ((&))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase)
import Opal.Error (ErrorAmbiguous (..), ErrorBadSyntax)

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

-- ParseError ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ParseError
  = ParseAmbiguous {-# UNPACK #-} !ErrorAmbiguous
    -- ^ TODO: docs
  | ParseBadSyntax {-# UNPACK #-} !ErrorBadSyntax
    -- ^ TODO: docs
  deriving (Show)

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

$(defineLenses ''ParseConfig)

-- | @since 1.0.0
instance Default ParseConfig where
  def = ParseConfig def def
