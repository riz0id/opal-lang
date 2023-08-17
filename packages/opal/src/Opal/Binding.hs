{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Binding
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
module Opal.Binding
  ( -- * Binding
    Binding (..)
    -- ** Optics
  , bindingSymbol
  , bindingScopes
  )
where

import Control.DeepSeq (NFData)

import GHC.Generics (Generic)

import Opal.Common.Lens (defineLenses)
import Opal.Common.Symbol (Symbol)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Writer.Class (Display (..))
import Opal.Writer.Doc qualified as Doc

-- Binding ---------------------------------------------------------------------

-- | 'Binding' represents a binding between a symbol and a set of scopes.
--
-- @since 1.0.0
data Binding = Binding
  { binding_scopes :: ScopeSet
    -- ^ The set of scopes bound to the symbol.
  , binding_symbol :: {-# UNPACK #-} !Symbol
    -- ^ The symbol that is bound.
  }
  deriving (Eq, Generic, Ord, Show)

$(defineLenses ''Binding)

-- | @since 1.0.0
instance Display Binding where
  display = Doc.string . show

-- | @since 1.0.0
instance NFData Binding
