{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
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
  , moduleToSyntax
    -- ** Optics
  , moduleName
  , moduleImports
  , moduleExports
  , moduleNamespace
  , moduleBasePhase
  , moduleDefinitions
    -- * Namespace
  , Namespace (..)
    -- ** Basic Operations
  , newNamespace
    -- ** Optics
  , namespaceBasePhase
  , namespaceDefinitions
  ) where

import Control.Lens (Lens', (^.))

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase, PhaseShift)
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax.Definition (Definition, definitionToSyntax)
import Opal.Syntax (Syntax, syntaxScope, Identifier)
import Opal.Syntax.TH (syntax)

-- Namespace -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Namespace = Namespace
  { namespace_base_phase  :: {-# UNPACK #-} !Phase
    -- ^ The namespace's base phase. This is the phase that is used by
    -- reflective operations like expansion and evaluation.
  , namespace_definitions :: [(PhaseShift, Definition)]
    -- ^ TODO: docs
  }
  deriving (Eq, Generic, Ord)

$(defineLenses ''Namespace)

-- | @since 1.0.0
instance Default Namespace where
  def = newNamespace def

-- | @since 1.0.0
instance Display Namespace where
  display (Namespace ph defns) =
    mconcat
      [ Doc.string "(namespace" <+> display ph
      , if null defns
          then Doc.char ')'
          else Doc.indent 2 (Doc.vsep (map (uncurry displayDefinition) defns) <> Doc.char ')')
      ]
    where
      displayDefinition :: PhaseShift -> Definition -> Doc
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

-- Module ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Module = Module
  { module_name      :: Identifier
    -- ^ The name of this module.
  , module_imports   :: [(Phase, Identifier)]
    -- ^ TODO: docs
  , module_exports   :: [(Phase, Identifier)]
    -- ^ TODO: docs
  , module_namespace :: {-# UNPACK #-} !Namespace
    -- ^ The 'Namespace' attached to this module.
  }
  deriving (Eq, Generic, Ord)

$(defineLenses ''Module)

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
      displayImport :: Phase -> Identifier -> Doc
      displayImport ph s = Doc.hsep [ "(import", display ph, display s <> Doc.char ')' ]

      displayExport :: Phase -> Identifier -> Doc
      displayExport ph s = Doc.hsep [ "(export", display ph, display s <> Doc.char ')' ]

-- | @since 1.0.0
instance Show Module where
  show = Doc.pretty 60 . display

-- Module - Basic Operations ---------------------------------------------------

-- | Construct a new 'Module' for a given module's name and the module's
-- 'Namespace'.
--
-- @since 1.0.0
newModule :: Identifier -> Namespace -> Module
newModule name ns =
  Module
    { module_name      = name
    , module_imports   = []
    , module_exports   = []
    , module_namespace = ns
    }

-- | TODO: docs
--
-- @since 1.0.0
moduleToSyntax :: Module -> Syntax
moduleToSyntax (Module name imports exports ns) =
  syntaxScope Nothing def [syntax|
    (module ?name:id
      (import ?importIds:id ...)
      (export ?exportIds:id ...)
      ?expr ...)
  |]
  where
    importIds :: [Identifier]
    importIds = map snd imports

    exportIds :: [Identifier]
    exportIds = map snd exports

    expr :: [Syntax]
    expr = map (definitionToSyntax . snd) (ns ^. namespaceDefinitions)

-- Module - Optics -------------------------------------------------------------

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
moduleDefinitions :: Lens' Module [(PhaseShift, Definition)]
moduleDefinitions = moduleNamespace . namespaceDefinitions
{-# INLINE moduleDefinitions #-}
