{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Writer.Monad
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
module Opal.Writer.Monad
  ( -- * Writer
    Writer (..)
    -- * WriterConfig
  , WriterConfig (..)
    -- ** Lenses
  , writerMaxColumn
  )
where

import Control.Lens (Lens', lens)

import Data.Default (Default (..))

-- Writer ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Writer a = Writer
  { unWriter ::
      WriterConfig ->
      a
  }

-- | @since 1.0.0
instance Functor Writer where
  fmap f (Writer g) = Writer \c -> f (g c)
  {-# INLINE fmap #-}

-- WriterConfig ----------------------------------------------------------------

-- | 'WriterConfig' is used as the read-only environment for the 'Writer' monad.
--
-- @since 1.0.0
data WriterConfig = WriterConfig
  { writer_max_column :: Maybe Int
    -- ^ TODO: docs
  }
  deriving (Show)

-- | @since 1.0.0
instance Default WriterConfig where
  def = WriterConfig (Just 80)

-- WriterConfig - Lenses -------------------------------------------------------

-- | Lens focusing on the 'writer_max_column' field of 'WriterConfig'.
--
-- @since 1.0.0
writerMaxColumn :: Lens' WriterConfig (Maybe Int)
writerMaxColumn = lens writer_max_column \s x -> s { writer_max_column = x }
{-# INLINE writerMaxColumn #-}
