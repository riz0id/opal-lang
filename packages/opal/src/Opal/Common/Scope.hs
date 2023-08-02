{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.Scope
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
module Opal.Common.Scope
  ( -- * MonadScope
    MonadScope (..)
    -- * Scope
  , Scope (..)
    -- ** Basic Operations
  , coreScope
  )
where

import Control.DeepSeq (NFData)

import Data.Default (Default (..))
import Data.IORef (IORef, newIORef, atomicModifyIORef')

import Language.Haskell.TH (Pat(..))
import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.TH (Pattern (..))
import Opal.Writer.Class (Display(..))
import Opal.Writer.Doc qualified as Doc (hsep, string)

import System.IO.Unsafe (unsafePerformIO)

-- MonadScope ------------------------------------------------------------------

sourceScope :: IORef Word
sourceScope = unsafePerformIO (newIORef 0)

-- | TODO: docs
--
-- @since 1.0.0
class Monad m => MonadScope m where
  newScope :: m Scope

-- | @since 1.0.0
instance MonadScope IO where
  newScope = do
    sid <- atomicModifyIORef' sourceScope \x -> (1 + x, x)
    pure (Scope sid)

-- Scope -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Scope = Scope Word
  deriving newtype (Eq, NFData, Ord, Show)
  deriving (Lift)

-- | @since 1.0.0
instance Bounded Scope where
  maxBound = Scope maxBound

  minBound = coreScope

-- | @since 1.0.0
instance Default Scope where
  def = coreScope

-- | @since 1.0.0
instance Display Scope where
  display (Scope ph) = Doc.hsep [Doc.string "scope", display ph]
  {-# INLINEABLE display #-}

-- | @since 1.0.0
instance Pattern Scope where
  liftPat (Scope sc) = fmap (\x -> ConP 'Scope [] [x]) (liftPat sc)

-- Scope - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
coreScope :: Scope
coreScope = Scope 0
