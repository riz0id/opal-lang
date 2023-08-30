{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Opal.Expander.Monad
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Expand' monad along with its associated read-only
-- state 'ExpandConfig'.
--
-- @since 1.0.0
module Opal.Expander.Monad
  ( -- * Expand
    Expand (..)
    -- ** Basic Operations
  , runExpand
  , lookupEnvironment
  , withTopLevelContext
  , withModuleContext
  , withModuleBeginContext
  , withDefinitionContext
  , withExpressionContext
    -- * ExpandConfig
  , ExpandConfig (..)
    -- ** Lenses
  , expandEnvironment
  , expandCurrentPhase
  , expandContext
  , expandFilePath
    -- * ExpandError
  , ExpandError (..)
    -- ** Basic Operations
  , throwBadSyntax
    -- * ExpandState
  , ExpandState (..)
    -- ** Basic Operations
  , defaultExpandState
    -- ** Lenses
  , expandBindingStore
  , expandNamespace
  , expandIntroScopes
  , expandUsageScopes
    -- * ExpansionContext
  , ExpansionContext (..)
    -- ** Basic Operations
  , expansionContextSymbol
  , expansionContextString
    -- * ExpansionLog
  , ExpansionLog (..)
    -- ** Basic Operations
  , writeLog
  , writeLogs
  )
where

import Control.Lens (set, use, view)

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT(..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))

import Data.Function ((&))

import GHC.Exts (RealWorld)

import Opal.Binding.Environment qualified as Environment
import Opal.Common.Scope (MonadScope (..))
import Opal.Common.Symbol (MonadGenSym (..))
import Opal.Error (ErrorNotBound (..))
import Opal.Resolve (ResolveError (..), resolve, MonadResolve, resolveId)
import Opal.Syntax
import Opal.Syntax.Transformer (Transformer(..))
import Opal.Expander.Error
  ( ExpandError (..)
  , throwBadSyntax
  )
import Opal.Expander.Config
  ( ExpandConfig (..)
  , ExpansionContext (..)
  , expandCurrentPhase
  , expandContext
  , expandFilePath
  , expansionContextString
  , expansionContextSymbol
  )
import Opal.Expander.Log
  ( ExpansionLog (..)
  , writeLog
  , writeLogs
  )
import Opal.Expander.State
  ( ExpandState(..)
  , defaultExpandState
  , expandBindingStore
  , expandEnvironment
  , expandIntroScopes
  , expandNamespace
  , expandUsageScopes
  )

import Prelude hiding (id)


-- Expand -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Expand a = Expand
  { unExpand :: ReaderT ExpandConfig (StateT ExpandState (ExceptT ExpandError (WriterT [ExpansionLog] IO))) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ExpandError
    , MonadReader ExpandConfig
    , MonadState ExpandState
    , MonadWriter [ExpansionLog]
    )

-- | @since 1.0.0
instance MonadGenSym Expand where
  newGenSym = liftIO newGenSym

-- | @since 1.0.0
instance MonadResolve Expand where
  resolve ph id = do
    store <- use expandBindingStore
    writeLog (LogResolveId id)
    case resolveId ph id store of
      Left  e -> throwError (resolveToExpandError e)
      Right s -> pure s
    where
      resolveToExpandError :: ResolveError -> ExpandError
      resolveToExpandError (ResolveAmbiguous  e) = ExpandAmbiguous e
      resolveToExpandError (ResolveNotInScope e) = ExpandNotInScope e

-- | @since 1.0.0
instance MonadScope Expand where
  newScope = liftIO newScope

-- | @since 1.0.0
instance PrimMonad Expand where
  type PrimState Expand = RealWorld

  primitive = liftIO . primitive

-- Expand - Basic Operations ----------------------------------------------------

-- | Run an 'Expand' computation with the given 'ExpandConfig' and initial
-- 'ExpandState'.
--
-- @since 1.0.0
runExpand ::
  ExpandConfig ->
  ExpandState ->
  Expand a ->
  IO (Either ExpandError (a, ExpandState), [ExpansionLog])
runExpand c s0 expand =
  unExpand expand
    & flip runReaderT c
    & flip runStateT s0
    & runExceptT
    & runWriterT

-- | TODO: docs
--
-- @since 1.0.0
lookupEnvironment :: Identifier -> Expand Transformer
lookupEnvironment id = do
  ph   <- view expandCurrentPhase
  bind <- resolve ph id
  env  <- use expandEnvironment
  case Environment.lookup bind env of
    Nothing -> throwError (ExpandNotBound (ErrorNotBound id bind))
    Just x  -> pure x

-- | TODO: docs
--
-- @since 1.0.0
withTopLevelContext :: Expand a -> Expand a
withTopLevelContext = local (set expandContext ContextTopLevel)

-- | TODO: docs
--
-- @since 1.0.0
withModuleContext :: Expand a -> Expand a
withModuleContext = local (set expandContext ContextModule)

-- | TODO: docs
--
-- @since 1.0.0
withModuleBeginContext :: Expand a -> Expand a
withModuleBeginContext = local (set expandContext ContextModuleBegin)

-- | TODO: docs
--
-- @since 1.0.0
withDefinitionContext :: Expand a -> Expand a
withDefinitionContext = local (set expandContext ContextDefinition)

-- | TODO: docs
--
-- @since 1.0.0
withExpressionContext :: Expand a -> Expand a
withExpressionContext = local (set expandContext ContextExpression)
