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

import Control.Lens (view, (^.))

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))

import Data.Function ((&))

import Opal.Error (ErrorAmbiguous (..), ErrorBadSyntax)
import Opal.Parser.Config
  ( ParseConfig (..)
  , parseBindingStore
  , parseCurrentPhase
  )
import Opal.Resolve
  ( ResolveError(..)
  , MonadResolve(..)
  , resolveId
  )
import Opal.Syntax

import Prelude hiding (id)

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

-- | @since 1.0.0
instance MonadResolve Parse where
  resolve ph id = do
    store <- view parseBindingStore
    case resolveId ph id store of
      Left  exn     -> case exn of
        ResolveAmbiguous  x -> throwError (ParseAmbiguous x)
        ResolveNotInScope _ -> pure (id ^. idtSymbol)
      Right gensym  -> pure gensym

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
