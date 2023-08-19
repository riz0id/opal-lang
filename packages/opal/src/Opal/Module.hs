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
  ( -- * Definitions
    Definitions (..)
    -- ** Optics
  , definitionsVariables
  , definitionsTransformers
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
  , nsTransformer
  , nsVariable
    -- * Module
  , Module (..)
    -- ** Basic Operations
  , newModule
  , newCoreModule
  , moduleToSyntax
    -- ** Optics
  , moduleName
  , moduleImports
  , moduleExports
  , moduleNamespace
  , moduleBasePhase
  , moduleDefinitions
    -- * Import
  , Import (..)
    -- * ImportSpec
  , ImportSpec (..)
  ) where

import Control.Applicative ((<|>))

import Control.Lens (Lens', lens, over, (^.))

import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)

import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase, PhaseShift)
import Opal.Common.Symbol (Symbol)
import Opal.Module.Import
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax.Definition (Definition, definitionToSyntax)
import Opal.Syntax (Syntax, syntaxScope, Identifier, Value)
import Opal.Syntax.TH (syntax)

import Prelude hiding (mod)

-- Definitions -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Definitions = Definitions
  { definitions_variables    :: Map Symbol Value
    -- ^ TODO: docs
  , definitions_transformers :: Map Symbol Value
    -- ^ TODO: docs
  }
  deriving (Eq, Generic, Ord)

$(defineLenses ''Definitions)

-- | @since 1.0.0
instance Default Definitions where
  def = defaultDefinitions

-- | @since 1.0.0
instance Monoid Definitions where
  mempty = defaultDefinitions

-- | @since 1.0.0
instance Semigroup Definitions where
  Definitions xs1 ys1 <> Definitions xs2 ys2 = Definitions (xs1 <> xs2) (ys1 <> ys2)

-- Definitions - Basic Operations ----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
defaultDefinitions :: Definitions
defaultDefinitions = Definitions Map.empty  Map.empty

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
  deriving (Eq, Generic, Ord)

-- | @since 1.0.0
instance Default Namespace where
  def = newNamespace def

-- | @since 1.0.0
instance Display Namespace where
  display (Namespace ph _ _ defns) =
    (Doc.parens . mconcat)
      [ Doc.string "namespace" <+> display ph
      , Doc.indent 2 (Doc.vsep (map (uncurry displayDefinition) undefined))
      ]
    where
      displayDefinition :: PhaseShift -> Definition -> Doc
      displayDefinition ph' s = Doc.char '(' <> display ph' <+> display s <> Doc.char ')'

-- | @since 1.0.0
instance Show Namespace where
  show = Doc.pretty . display

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
declareModule :: Namespace -> Symbol -> Module -> Bool -> Namespace
declareModule ns s mod asSubmodule
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
    setter phs defns = Map.insertWith (flip (<>)) ph defns phs

-- | TODO: docs
--
-- @since 1.0.0
nsTransformer :: Phase -> Symbol -> Lens' Namespace Value
nsTransformer ph s = nsDefinitions ph . definitionsTransformers . lens getter setter
  where
    getter :: Map Symbol Value -> Value
    getter = fromMaybe (error "unimplemented: #void") . Map.lookup s -- FIXME: implement #void datum

    setter :: Map Symbol Value -> Value -> Map Symbol Value
    setter vals val = Map.insert s val vals

-- | TODO: docs
--
-- @since 1.0.0
nsVariable :: Phase -> Symbol -> Lens' Namespace Value
nsVariable ph s = nsDefinitions ph . definitionsVariables . lens getter setter
  where
    getter :: Map Symbol Value -> Value
    getter = fromMaybe (error "unimplemented: #void") . Map.lookup s -- FIXME: implement #void datum

    setter :: Map Symbol Value -> Value -> Map Symbol Value
    setter vals val = Map.insert s val vals

-- Module ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Module = Module
  { module_name      :: Symbol
    -- ^ The name of this module.
  , module_imports   :: [Import]
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
      , (Doc.indent 2 . Doc.vsep)
          [ display imports
          , Doc.hsep (map (uncurry displayExport) exports)
          , display ns <> Doc.char ')'
          ]
      ]
    where
      displayExport :: Phase -> Identifier -> Doc
      displayExport ph s = Doc.hsep [ "(export", display ph, display s <> Doc.char ')' ]

-- | @since 1.0.0
instance Show Module where
  show = Doc.pretty . display

-- Module - Basic Operations ---------------------------------------------------

-- | Construct a new 'Module' for a given module's name and the module's
-- 'Namespace'.
--
-- @since 1.0.0
newModule :: Symbol -> Namespace -> Module
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
newCoreModule :: Namespace -> Module
newCoreModule ns =
  Module
    { module_name      = "opal-core"
    , module_imports   = []
    , module_exports   = undefined
    , module_namespace = undefined
    }

-- | TODO: docs
--
-- @since 1.0.0
moduleToSyntax :: Module -> Syntax
moduleToSyntax (Module name imports exports ns) =
  syntaxScope Nothing def [syntax|
    (module ?name:symbol
      ?importStxs ...
      (export ?exportIds:id ...)
      ?exprStxs ...)
  |]
  where
    importStxs :: [Syntax]
    importStxs = map importToSyntax imports

    exportIds :: [Identifier]
    exportIds = map snd exports

    exprStxs :: [Syntax]
    exprStxs = map (definitionToSyntax . snd) (ns ^. undefined)

-- Module - Optics -------------------------------------------------------------

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceBasePhase')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleBasePhase :: Lens' Module Phase
moduleBasePhase = moduleNamespace . nsBasePhase
{-# INLINE moduleBasePhase #-}

-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceDefinitions')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleDefinitions :: Lens' Module [(PhaseShift, Definition)]
moduleDefinitions = moduleNamespace . undefined
{-# INLINE moduleDefinitions #-}
