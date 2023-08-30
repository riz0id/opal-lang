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
  , defaultExpandConfig
    -- ** Lenses
  , expandCurrentPhase
  , expandContext
  , expandFilePath
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
  def = ContextDefinition

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
  , expand_file_path     :: Maybe FilePath
    -- ^ If given, 'expand_file_path' the path to the file the module is
    -- currently expanding. Otherwise, the expander is running in a REPL.
  }
  deriving (Generic, Show)

$(defineLenses ''ExpandConfig)

-- | 'ExpandConfig' defaults to 'defaultExpandConfig'.
--
-- @since 1.0.0
instance Default ExpandConfig where
  def = defaultExpandConfig

-- ExpandConfig - Basic Operations ---------------------------------------------

-- | The default 'ExpandConfig'.
--
-- @since 1.0.0
defaultExpandConfig :: ExpandConfig
defaultExpandConfig = ExpandConfig def def def