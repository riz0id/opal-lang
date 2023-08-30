{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Module.Definitions
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
module Opal.Module.Definitions
  ( -- * Definitions
    Definitions (..)
    -- ** Basic Operations
  , defaultDefinitions
  , definitionsToSyntaxes
    -- ** Optics
  , defnsVariables
  , defnsTransformers
  , defnsVariable
  , defnsTransformer
  ) where


import Control.Lens (Lens', at, (^.))

import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import GHC.Generics (Generic)

import Opal.Common.Lens (defineLenses)
import Opal.Common.Symbol (Symbol)
import Opal.Core (CoreForm (..), coreFormIdentifier)
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax.Definition
  ( Define (..)
  , DefineSyntax (..)
  , defineToSyntax
  , defineSyntaxToSyntax
  )
import Opal.Syntax (Identifier (..), Syntax (..), datumToSyntax, syntaxCons, identifierToSyntax)
import Opal.Syntax.Transformer (Transformer (..))

import Prelude hiding (id, mod)

-- Definitions -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Definitions = Definitions
  { defns_transformers :: Map Symbol Transformer
    -- ^ TODO: docs
  , defns_variables    :: Map Symbol Transformer
    -- ^ TODO: docs
  }
  deriving (Eq, Generic, Ord, Show)

$(defineLenses ''Definitions)

-- | 'Definitions' defaults to 'defaultDefinitions'.
--
-- @since 1.0.0
instance Default Definitions where
  def = defaultDefinitions

-- | @since 1.0.0
instance Display Definitions where
  display (Definitions trans vals) =
    (Doc.parens . mconcat)
      [ Doc.string "definitions"
      , (Doc.indent 2 . Doc.parens . mconcat)
          [ "begin-for-syntax"
          , Doc.indent 2 (Doc.vsep (displayDefns trans))
          ]
      , (Doc.indent 2 . Doc.parens . mconcat)
          [ "begin"
          , Doc.indent 2 (Doc.vsep (displayDefns vals))
          ]
      ]
    where
      displayDefns :: Map Symbol Transformer -> [Doc]
      displayDefns = Map.foldrWithKey (\id t xs -> displayDefn id t : xs) mempty

      displayDefn :: Symbol -> Transformer -> Doc
      displayDefn s (TfmCore form) =
        (Doc.parens . Doc.hsep)
          [ "define"
          , display s
          , Doc.parens ("#%built-in" <+> display form)
          ]
      displayDefn id (TfmDatum val) =
        (Doc.parens . Doc.hsep)
          [ "define"
          , display id
          , display val
          ]

-- | @since 1.0.0
instance Monoid Definitions where
  mempty = defaultDefinitions

-- | @since 1.0.0
instance Semigroup Definitions where
  Definitions xs1 ys1 <> Definitions xs2 ys2 = Definitions (xs1 <> xs2) (ys1 <> ys2)

-- Definitions - Basic Operations ----------------------------------------------

-- | The default or empty 'Definitions'.
--
-- @since 1.0.0
defaultDefinitions :: Definitions
defaultDefinitions = Definitions Map.empty Map.empty

-- | TODO: docs
--
-- @since 1.0.0
definitionsToSyntaxes :: Definitions -> [Syntax]
definitionsToSyntaxes defns =
  let transDefns = Map.foldrWithKey (\k x xs -> syntaxDefnToSyntax k x : xs) [] (defns ^. defnsTransformers)
      valDefns   = Map.foldrWithKey (\k x xs -> defnToSyntax k x : xs) [] (defns ^. defnsVariables)
   in transDefns ++ valDefns
  where
    syntaxDefnToSyntax :: Symbol -> Transformer -> Syntax
    syntaxDefnToSyntax s (TfmCore form) = undefined
      let stxCoreFormId :: Syntax
          stxCoreFormId = identifierToSyntax (coreFormIdentifier form)
       in identifierToSyntax (coreFormIdentifier CoreDefineSyntax)
            `syntaxCons` SyntaxS s def
            `syntaxCons` (SyntaxS "#%builtin" def `syntaxCons` stxCoreFormId)
    syntaxDefnToSyntax s (TfmDatum val) =
      defineSyntaxToSyntax (DefineSyntax (Identifier s def) (datumToSyntax def val))

    defnToSyntax :: Symbol -> Transformer -> Syntax
    defnToSyntax s (TfmCore form) =
      let stxCoreFormId :: Syntax
          stxCoreFormId = identifierToSyntax (coreFormIdentifier form)
       in identifierToSyntax (coreFormIdentifier CoreDefine)
            `syntaxCons` SyntaxS s def
            `syntaxCons` (SyntaxS "#%builtin" def `syntaxCons` stxCoreFormId)
    defnToSyntax s (TfmDatum val) =
      defineToSyntax (Define (Identifier s def) (datumToSyntax def val))

-- Definitions - Optics --------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
defnsVariable :: Symbol -> Lens' Definitions (Maybe Transformer)
defnsVariable s = defnsVariables . at s

-- | TODO: docs
--
-- @since 1.0.0
defnsTransformer :: Symbol -> Lens' Definitions (Maybe Transformer)
defnsTransformer s = defnsTransformers . at s