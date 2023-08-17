{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander.Config
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
module Opal.Expander.Config
  ( -- * ExpansionContext
    ExpansionContext (..)
    -- ** Basic Operations
  , expansionContextSymbol
  , expansionContextString
    -- * ExpandConfig
  , ExpandConfig (..)
    -- ** Basic Operations
  , coreExpandConfig
    -- ** Lenses
  , expandCurrentPhase
  , expandContext
  )
where

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase)
import Opal.Common.Symbol (Symbol, stringToSymbol)
import Opal.Writer (Display (..))
import Opal.Writer qualified as Doc

import Prelude hiding (id)

-- ExpansionContext ------------------------------------------------------------

-- | 'ExpansionContext' is an enumeration of the various expansion contexts.
--
-- @since 1.0.0
data ExpansionContext
  = ContextDefinition
    -- ^ The definition-level expansion context.
  | ContextExpression
    -- ^ The expression-level expansion context.
  | ContextModule
    -- ^ The module-level expansion context.
  | ContextModuleBegin
    -- ^ The module body expansion context.
  | ContextTopLevel
    -- ^ The top-level expansion context.
  deriving (Bounded, Enum, Eq, Ord)

-- | @since 1.0.0
instance Default ExpansionContext where
  def = ContextTopLevel

-- | @since 1.0.0
instance Display ExpansionContext where
  display = Doc.string . expansionContextString

-- | @since 1.0.0
instance Show ExpansionContext where
  show = Doc.pretty . display

-- ExpansionContext ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expansionContextSymbol :: ExpansionContext -> Symbol
expansionContextSymbol = stringToSymbol . expansionContextString

-- | TODO: docs
--
-- @since 1.0.0
expansionContextString :: ExpansionContext -> String
expansionContextString ContextExpression  = "expression"
expansionContextString ContextTopLevel    = "top-level"
expansionContextString ContextModule      = "module"
expansionContextString ContextModuleBegin = "module-begin"
expansionContextString ContextDefinition  = "definition"

-- ExpandConfig -----------------------------------------------------------------

-- | 'ExpandConfig' is the read-only state of the 'Expand' monad.
--
-- @since 1.0.0
data ExpandConfig = ExpandConfig
  { expand_current_phase :: {-# UNPACK #-} !Phase
    -- ^ The current expansion phase.
  , expand_context       :: ExpansionContext
    -- ^ The current expansion context.
  }
  deriving (Generic, Show)

$(defineLenses ''ExpandConfig)

-- | 'ExpandConfig' defaults to 'coreExpandConfig'.
--
-- @since 1.0.0
instance Default ExpandConfig where
  def = coreExpandConfig

-- ExpandConfig - Basic Operations ---------------------------------------------

-- | The default 'ExpandConfig' with bindings for the core syntactic forms.
--
-- @since 1.0.0
coreExpandConfig :: ExpandConfig
coreExpandConfig =
  ExpandConfig
    { expand_current_phase = def
    , expand_context       = def
    }

