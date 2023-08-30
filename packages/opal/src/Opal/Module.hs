{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
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
  ( -- * Definitions
    Definitions (..)
    -- ** Optics
  , defnsVariables
  , defnsTransformers
  , defnsVariable
  , defnsTransformer
    -- * Namespace
  , Namespace (..)
    -- ** Basic Operations
  , newNamespace
  , nsToModule
  , declareModule
    -- ** Optics
  , nsBasePhase
  , nsModuleDeclarations
  , nsSubmoduleDeclarations
  , nsPhases
  , nsDefinitions
  -- , nsBinding
  , nsBinding
  , nsTransformer
  , nsVariable
    -- * Module
  , Module (..)
    -- ** Basic Operations
  , newModule
  , newCoreModule
  , moduleExportPhaseLevels
  , moduleImportPhaseLevels
  , moduleToSyntax
    -- ** Optics
  , moduleName
  , moduleImports
  , moduleExports
  , moduleNamespace
  , moduleBasePhase
  , moduleBinding
  , moduleDefinitions
    -- * Import
  , Import (..)
    -- * ImportSpec
  , ImportSpec (..)
  ) where

import Control.Applicative ((<|>))

import Control.Lens (Lens', lens, over, view, (.~), (^.), at)

import Data.Default (Default (..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)

import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase, PhaseShift)
import Opal.Common.Symbol (Symbol)
import Opal.Core (CoreForm (..), coreFormIdentifier, coreFormSymbol)
import Opal.Module.Definitions
import Opal.Module.Import
import Opal.Module.Export
import Opal.Writer (Display (..), (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax.Definition (Definition)
import Opal.Syntax (Identifier, Syntax, syntaxScope, Datum (..))
import Opal.Syntax.TH (syntax)
import Opal.Syntax.Transformer (Transformer (..))

import Prelude hiding (id, mod)

-- Namespace -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Namespace = Namespace
  { ns_base_phase             :: {-# UNPACK #-} !Phase
    -- ^ The namespace's base phase. This is the phase that is used by
    -- reflective operations like expansion and evaluation.
  , ns_module_declarations    :: Map Symbol Module
    -- ^ TODO: docs
  , ns_submodule_declarations :: Map Symbol Module
    -- ^ TODO: docs
  , ns_phases                 :: Map Phase Definitions
    -- ^ TODO: docs
  }
  deriving (Eq, Generic, Ord, Show)

-- | @since 1.0.0
instance Default Namespace where
  def = newNamespace def

-- | @since 1.0.0
instance Display Namespace where
  display (Namespace ph _ _ defns) =
    (Doc.parens . Doc.hsep)
      [ "namespace"
      , display ph
      , display (mconcat (Map.elems defns))
      ]

-- Namespace - Basic Operations ------------------------------------------------

-- | Construct a new 'Namespace' for a given base phase.
--
-- @since 1.0.0
newNamespace :: Phase -> Namespace
newNamespace ph = Namespace ph Map.empty Map.empty def

-- | TODO: docs
--
-- @since 1.0.0
nsToModule :: Symbol -> Namespace -> Maybe Module
nsToModule s ns = Map.lookup s (ns ^. nsModuleDeclarations) <|> Map.lookup s (ns ^. nsSubmoduleDeclarations)

-- | TODO: docs
--
-- @since 1.0.0
declareModule :: Symbol -> Module -> Bool -> Namespace -> Namespace
declareModule s mod asSubmodule ns
  | asSubmodule = over nsSubmoduleDeclarations (Map.insert s mod) ns
  | otherwise   = over nsModuleDeclarations (Map.insert s mod) ns

-- Namespace - Optics ----------------------------------------------------------

-- | Lens focusing on the 'ns_base_phase' field of a 'Namespace'.
--
-- @since 1.0.0
nsBasePhase :: Lens' Namespace Phase
nsBasePhase = lens ns_base_phase \s x -> s { ns_base_phase = x }

-- | Lens focusing on the 'ns_module_declarations' field of a 'Namespace'.
--
-- @since 1.0.0
nsModuleDeclarations :: Lens' Namespace (Map Symbol Module)
nsModuleDeclarations = lens ns_module_declarations \s x -> s { ns_module_declarations = x }

-- | Lens focusing on the 'ns_submodule_declarations' field of a 'Namespace'.
--
-- @since 1.0.0
nsSubmoduleDeclarations :: Lens' Namespace (Map Symbol Module)
nsSubmoduleDeclarations = lens ns_submodule_declarations \s x -> s { ns_submodule_declarations = x }

-- | Lens focusing on the 'ns_phases' field of a 'Namespace'.
--
-- @since 1.0.0
nsPhases :: Lens' Namespace (Map Phase Definitions)
nsPhases = lens ns_phases \s x -> s { ns_phases = x }

-- | TODO: docs
--
-- @since 1.0.0
nsDefinitions :: Phase -> Lens' Namespace Definitions
nsDefinitions ph = nsPhases . lens getter setter
  where
    getter :: Map Phase Definitions -> Definitions
    getter = fromMaybe def . Map.lookup ph

    setter :: Map Phase Definitions -> Definitions -> Map Phase Definitions
    setter phs defns = Map.insertWith ((<>)) ph defns phs

-- | TODO: docs
--
-- @since 1.0.0
nsBinding :: Phase -> Symbol -> Lens' Namespace (Maybe Transformer)
nsBinding ph id
  | ph == def = nsVariable ph id
  | otherwise = nsTransformer ph id

-- | TODO: docs
--
-- @since 1.0.0
nsTransformer :: Phase -> Symbol -> Lens' Namespace (Maybe Transformer)
nsTransformer ph id = nsDefinitions ph . defnsTransformers . at id

-- | TODO: docs
--
-- @since 1.0.0
nsVariable :: Phase -> Symbol -> Lens' Namespace (Maybe Transformer)
nsVariable ph id = nsDefinitions ph . defnsVariables . at id

-- Module ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Module = Module
  { module_name      :: Symbol
    -- ^ The name of this module.
  , module_imports   :: Import
    -- ^ TODO: docs
  , module_exports   :: Export
    -- ^ TODO: docs
  , module_namespace :: {-# UNPACK #-} !Namespace
    -- ^ The 'Namespace' attached to this module.
  }
  deriving (Eq, Generic, Ord, Show)

$(defineLenses ''Module)

-- | @since 1.0.0
instance Display Module where
  display (Module name imports exports ns) =
    (Doc.parens . Doc.hsep)
      [ Doc.group ("module" <+> display name)
      , (Doc.indent 2 . Doc.vsep)
          [ display imports
          , display exports
          , display ns
          ]
      ]

-- Module - Basic Operations ---------------------------------------------------

-- | Construct a new 'Module' for a given module's name and the module's
-- 'Namespace'.
--
-- @since 1.0.0
newModule :: Symbol -> Namespace -> Module
newModule name ns =
  Module
    { module_name      = name
    , module_imports   = def
    , module_exports   = def
    , module_namespace = ns
    }

-- | TODO: docs
--
-- @since 1.0.0
newCoreModule :: Namespace -> Module
newCoreModule ns =
  Module
    { module_name      = "#%core"
    , module_imports   = def
    , module_exports   = Export def coreExportSpec
    , module_namespace = coreNamespace
    }
  where
    coreForms :: [CoreForm]
    coreForms = [minBound .. maxBound]

    coreExportSpec :: [ExportSpec]
    coreExportSpec = map (ExportSpecPhaseless . coreFormIdentifier) coreForms

    coreNamespace :: Namespace
    coreNamespace = foldr (\form -> over (nsVariable def (coreFormSymbol form)) (const (Just (TfmCore form)))) ns coreForms

-- | TODO: docs
--
-- @since 1.0.0
moduleExportPhaseLevels :: Module -> [(PhaseShift, Identifier)]
moduleExportPhaseLevels = exportPhaseLevels . view moduleExports

-- | TODO: docs
--
-- @since 1.0.0
moduleImportPhaseLevels :: Module -> [(PhaseShift, Symbol)]
moduleImportPhaseLevels = importPhaseLevels . view moduleImports

-- | TODO: docs
--
-- @since 1.0.0
moduleToSyntax :: Module -> Syntax
moduleToSyntax (Module name imports exports ns) =
  [syntax|
    (?moduleId ?name:symbol
      ?importStx
      ?exportStx
      ?exprStxs ...)
  |]
  where
    moduleId :: Syntax
    moduleId = syntaxScope Nothing def [syntax| module |]

    importStx :: Syntax
    importStx = importToSyntax imports

    exportStx :: Syntax
    exportStx = exportToSyntax exports

    exprStxs :: [Syntax]
    exprStxs = definitionsToSyntaxes (ns ^. nsDefinitions (ns ^. nsBasePhase))

-- Module - Optics -------------------------------------------------------------

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceBasePhase')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleBasePhase :: Lens' Module Phase
moduleBasePhase = moduleNamespace . nsBasePhase

-- | TODO: docs
--
-- @since 1.0.0
moduleBinding :: Phase -> Symbol -> Lens' Module (Maybe Transformer)
moduleBinding ph id = moduleNamespace . nsBinding ph id

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceDefinitions')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleDefinitions :: Lens' Module [(PhaseShift, Definition)]
moduleDefinitions = moduleNamespace . undefined
