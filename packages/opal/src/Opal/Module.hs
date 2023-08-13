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
    -- ** Optics
  , moduleName
  , moduleImports
  , moduleExports
  , moduleNamespace
  , moduleBasePhase
  , moduleDefinitions
  , -- * ModuleName
    ModuleName (..)
    -- * Namespace
  , Namespace (..)
    -- ** Basic Operations
  , newNamespace
    -- ** Optics
  , namespaceBasePhase
  , namespaceDefinitions
  ) where

import Control.Lens (Lens', lens)

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Common.Phase (Phase)
import Opal.Common.Symbol (Symbol)
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax.Definition (Definition)

-- Module ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Module = Module
  { module_name      :: ModuleName
    -- ^ The name of this module.
  , module_imports   :: [(Phase, Symbol)]
    -- ^ TODO: docs
  , module_exports   :: [(Phase, Symbol)]
    -- ^ TODO: docs
  , module_namespace :: {-# UNPACK #-} !Namespace
    -- ^ The 'Namespace' attached to this module.
  }
  deriving (Eq, Generic, Ord)

-- | @since 1.0.0
instance Display Module where
  display (Module name imports exports ns) =
    Doc.hsep
      [ Doc.string "(module"
      , display name
      , (Doc.nest 2 . mappend Doc.line . Doc.vsep)
          [ Doc.hsep (map (uncurry displayImport) imports)
          , Doc.hsep (map (uncurry displayExport) exports)
          , display ns <> Doc.char ')'
          ]
      ]
    where
      displayImport :: Phase -> Symbol -> Doc
      displayImport ph s = Doc.hsep [ "(import", display ph, display s <> Doc.char ')' ]

      displayExport :: Phase -> Symbol -> Doc
      displayExport ph s = Doc.hsep [ "(export", display ph, display s <> Doc.char ')' ]

-- | @since 1.0.0
instance Show Module where
  show = Doc.pretty 60 . display

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
    , module_exports   = []
    , module_namespace = ns
    }

-- Module - Optics -------------------------------------------------------------

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

-- | Lens focusing on the 'module_exports' of a 'Module'.
--
-- @since 1.0.0
moduleExports :: Lens' Module [(Phase, Symbol)]
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

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceDefinitions')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleDefinitions :: Lens' Module [(Phase, Definition)]
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
  deriving (Eq, Generic, Ord)

-- | @since 1.0.0
instance Display ModuleName where
  display (ModuleName s) = display s
  display ModuleTopLevel = Doc.string "#%top-level"

-- | @since 1.0.0
instance Show ModuleName where
  show = Doc.pretty 80 . display

-- Namespace -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Namespace = Namespace
  { namespace_base_phase  :: {-# UNPACK #-} !Phase
    -- ^ The namespace's base phase. This is the phase that is used by
    -- reflective operations like expansion and evaluation.
  , namespace_definitions :: [(Phase, Definition)]
    -- ^ TODO: docs
  }
  deriving (Eq, Generic, Ord)

-- | @since 1.0.0
instance Default Namespace where
  def = newNamespace def

-- | @since 1.0.0
instance Display Namespace where
  display (Namespace ph defns) =
    Doc.hsep
      [ Doc.string "(namespace"
      , display ph <> if null defns
          then Doc.char ')'
          else Doc.nest 2 (Doc.line <> Doc.vsep (map (uncurry displayDefinition) defns) <> Doc.char ')')
      ]
    where
      displayDefinition :: Phase -> Definition -> Doc
      displayDefinition ph' s = Doc.char '(' <> display ph' <+> display s <> Doc.char ')'

-- | @since 1.0.0
instance Show Namespace where
  show = Doc.pretty 80 . display

-- Namespace - Basic Operations ------------------------------------------------

-- | Construct a new 'Namespace' for a given base phase.
--
-- @since 1.0.0
newNamespace :: Phase -> Namespace
newNamespace ph =
  Namespace
    { namespace_base_phase  = ph
    , namespace_definitions = def
    }

-- Namespace - Optics ----------------------------------------------------------

-- | Lens focusing on the 'namespace_base_phase' of a 'Module'.
--
-- @since 1.0.0
namespaceBasePhase :: Lens' Namespace Phase
namespaceBasePhase = lens namespace_base_phase \s x -> s { namespace_base_phase = x }
{-# INLINE namespaceBasePhase #-}

-- | Lens focusing on the 'namespace_definitions' of a 'Module'.
--
-- @since 1.0.0
namespaceDefinitions :: Lens' Namespace [(Phase, Definition)]
namespaceDefinitions = lens namespace_definitions \s x -> s { namespace_definitions = x }
{-# INLINE namespaceDefinitions #-}