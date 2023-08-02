{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Module
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
module Opal.Module
  ( -- * Module
    Module (..)
    -- ** Basic Operations
  , newModule
    -- ** Lenses
  , moduleName
  , moduleImports
  , moduleExports
  , moduleNamespace
  , moduleBasePhase
  , moduleRegistry
  , moduleDefinitions
  , -- * ModuleName
    ModuleName (..)
  , -- * ModuleRegistry
    ModuleRegistry (..)
    -- ** Basic Operations
  , emptyModuleRegistry
    -- * Namespace
  , Namespace (..)
    -- ** Basic Operations
  , newNamespace
    -- ** Lenses
  , namespaceBasePhase
  , namespaceRegistry
  , namespaceDefinitions
  ) where

import Control.Lens (Lens', lens)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import GHC.Generics (Generic)

import Opal.Common.Phase (Phase)
import Opal.Common.Symbol (Symbol)
import Opal.Syntax (Syntax)

-- Module ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Module = Module
  { module_name      :: ModuleName
    -- ^ The name of this module.
  , module_imports   :: [(Phase, Symbol)]
    -- ^ TODO: docs
  , module_exports   :: Map Phase Symbol
    -- ^ TODO: docs
  , module_namespace :: {-# UNPACK #-} !Namespace
    -- ^ The 'Namespace' attached to this module.
  }
  deriving (Eq, Generic, Ord, Show)

-- Module - Basic Operations ---------------------------------------------------

-- | Construct a new 'Module' for a given module's name and the module's
-- 'Namespace'.
--
-- @since 1.0.0
newModule :: ModuleName -> Namespace -> Module
newModule name ns =
  Module
    { module_name      = name
    , module_imports   = []
    , module_exports   = Map.empty
    , module_namespace = ns
    }

-- Module - Lenses -------------------------------------------------------------

-- | Lens focusing on the 'module_name' of a 'Module'.
--
-- @since 1.0.0
moduleName :: Lens' Module ModuleName
moduleName = lens module_name \s x -> s { module_name = x }
{-# INLINE moduleName #-}

-- | Lens focusing on the 'module_imports' of a 'Module'.
--
-- @since 1.0.0
moduleImports :: Lens' Module [(Phase, Symbol)]
moduleImports = lens module_imports \s x -> s { module_imports = x }
{-# INLINE moduleImports #-}

-- | Lens focusing on the 'module_imports' of a 'Module'.
--
-- @since 1.0.0
moduleExports :: Lens' Module (Map Phase Symbol)
moduleExports = lens module_exports \s x -> s { module_exports = x }
{-# INLINE moduleExports #-}

-- | Lens focusing on the 'module_namespace' of a 'Module'.
--
-- @since 1.0.0
moduleNamespace :: Lens' Module Namespace
moduleNamespace = lens module_namespace \s x -> s { module_namespace = x }
{-# INLINE moduleNamespace #-}

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceBasePhase')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleBasePhase :: Lens' Module Phase
moduleBasePhase = moduleNamespace . namespaceBasePhase
{-# INLINE moduleBasePhase #-}

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceRegistry')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleRegistry :: Lens' Module ModuleRegistry
moduleRegistry = moduleNamespace . namespaceRegistry
{-# INLINE moduleRegistry #-}

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceDefinitions')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleDefinitions :: Lens' Module (Map Phase [Syntax])
moduleDefinitions = moduleNamespace . namespaceDefinitions
{-# INLINE moduleDefinitions #-}

-- ModuleName ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ModuleName
  = ModuleName {-# UNPACK #-} !Symbol
  -- ^ TODO: docs
  | ModuleTopLevel
  -- ^ TODO: docs
  deriving (Eq, Generic, Ord, Show)

-- ModuleRegistry --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype ModuleRegistry
  = ModuleRegistry { getModuleRegistry :: HashMap Symbol Module }
  deriving (Eq, Generic, Ord, Show)

-- ModuleRegistry - Basic Operations -------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
emptyModuleRegistry :: ModuleRegistry
emptyModuleRegistry = ModuleRegistry HashMap.empty

-- Namespace -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Namespace = Namespace
  { namespace_base_phase  :: {-# UNPACK #-} !Phase
    -- ^ The namespace's base phase. This is the phase that are used by
    -- reflective operations like expansion and evaluation.
  , namespace_registry    :: ModuleRegistry
    -- ^ TODO: docs
  , namespace_definitions :: Map Phase [Syntax]
    -- ^ TODO: docs
  }
  deriving (Eq, Generic, Ord, Show)

-- Namespace - Basic Operations ------------------------------------------------

-- | Construct a new 'Namespace' for a given base phase.
--
-- @since 1.0.0
newNamespace :: Phase -> Namespace
newNamespace ph =
  Namespace
    { namespace_base_phase  = ph
    , namespace_definitions = Map.empty
    , namespace_registry    = emptyModuleRegistry
    }

-- Namespace - Lenses ----------------------------------------------------------

-- | Lens focusing on the 'namespace_base_phase' of a 'Module'.
--
-- @since 1.0.0
namespaceBasePhase :: Lens' Namespace Phase
namespaceBasePhase = lens namespace_base_phase \s x -> s { namespace_base_phase = x }
{-# INLINE namespaceBasePhase #-}

-- | Lens focusing on the 'module_name' of a 'Module'.
--
-- @since 1.0.0
namespaceRegistry :: Lens' Namespace ModuleRegistry
namespaceRegistry = lens namespace_registry \s x -> s { namespace_registry = x }
{-# INLINE namespaceRegistry #-}

-- | Lens focusing on the 'namespace_definitions' of a 'Module'.
--
-- @since 1.0.0
namespaceDefinitions :: Lens' Namespace (Map Phase [Syntax])
namespaceDefinitions = lens namespace_definitions \s x -> s { namespace_definitions = x }
{-# INLINE namespaceDefinitions #-}