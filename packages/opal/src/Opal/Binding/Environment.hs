{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Binding.Environment
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
module Opal.Binding.Environment
  ( -- * Environment
    Environment (..)
    -- ** Construct
  , empty
  , singleton
  , coreEnvironment
    -- ** Insert
  , insert
    -- ** Delete
  , delete
    -- ** Lookup
  , lookup
    -- ** Query
  , null
  , size
  )
where

import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

import Opal.Common.Symbol (Symbol)
import Opal.Core (coreFormSymbol)
import Opal.Syntax.Transformer (Transformer (..))
import Opal.Writer.Class (Display (..))
import Opal.Writer.Doc qualified as Doc

import Prelude hiding (filter, lookup, null)

-- Environment -----------------------------------------------------------------

-- | 'Environment' is a map from generated symbols to syntax transformers.
--
-- @since 1.0.0
newtype Environment = Environment
  { getEnvironment :: Map Symbol Transformer }
  deriving stock   (Generic)
  deriving newtype (Show)

-- | 'Environment' defaults to 'empty'.
--
-- @since 1.0.0
instance Default Environment where
  def = empty

-- | @since 1.0.0
instance Display Environment where
  display = Doc.string . show

-- | @since 1.0.0
instance IsList Environment where
  type Item Environment = (Symbol, Transformer)

  fromList = coerce @(_ -> Map _ Transformer) Map.fromList

  toList = coerce @(Map _ Transformer -> _) Map.toList

-- Environment - Construct ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
empty :: Environment
empty = Environment Map.empty

-- | TODO: docs
--
-- @since 1.0.0
singleton :: Symbol -> Transformer -> Environment
singleton = coerce @(_ -> Transformer -> _) Map.singleton

-- | TODO: docs
--
-- @since 1.0.0
coreEnvironment :: Environment
coreEnvironment = fromList [ (coreFormSymbol x, TfmCore x) | x <- [minBound .. maxBound] ]

-- Environment - Insert -------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
insert :: Symbol -> Transformer -> Environment -> Environment
insert = coerce @(_ -> Transformer -> _) Map.insert

-- Environment - Delete -------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
delete :: Symbol -> Environment -> Environment
delete = coerce @(_ -> Map _ Transformer -> _) Map.delete

-- Environment - Lookup -------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
lookup :: Symbol -> Environment -> Maybe Transformer
lookup = coerce @(_ -> Map _ Transformer -> _) Map.lookup

-- Environment - Query --------------------------------------------------------

-- | Is the given 'Environment' empty?
--
-- @since 1.0.0
null :: Environment -> Bool
null = coerce @(Map _ Transformer -> _) Map.null

-- | Obtain the size of the given 'Environment'.
--
-- @since 1.0.0
size :: Environment -> Int
size = coerce @(Map _ Transformer -> _) Map.size