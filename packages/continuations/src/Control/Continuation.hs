{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Continuation
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
module Control.Continuation
  ( -- * PromptTag
    PromptTag (..),
    -- ** Basic Operations
    newPromptTag,
    -- * Continuations
    prompt,
    control0,
  )
where

import GHC.Exts (PromptTag#, RealWorld, State#)
import GHC.Exts qualified as GHC
import GHC.IO (IO (..))

--------------------------------------------------------------------------------

unIO# :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO# (IO io#) = io#

-- PromptTag -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data PromptTag a = PromptTag (PromptTag# a)

-- | @since 1.0.0
instance Eq (PromptTag a) where
  PromptTag a# == PromptTag b# = GHC.isTrue# (GHC.samePromptTag# a# b#)

-- PromptTag - Basic Operations ------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newPromptTag :: IO (PromptTag a)
newPromptTag =
  IO \rw0# -> case GHC.newPromptTag# rw0# of
    (# rw1#, tag# #) -> (# rw1#, PromptTag tag# #)

-- Continuations ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag tag#) (IO io#) = IO (GHC.prompt# tag# io#)

-- | TODO: docs
--
-- @since 1.0.0
control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag tag#) k = IO (GHC.control0# tag# \k# -> unIO# (k (IO . k# . unIO#)))