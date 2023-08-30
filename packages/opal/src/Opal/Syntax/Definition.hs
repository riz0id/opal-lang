{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.Definition
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
module Opal.Syntax.Definition
  ( Definition (..)
    -- ** Basic Operations
  , definitionToSyntax
    -- * Begin
  , Begin (..)
    -- ** Basic Operations
  , beginToDefinitions
  , beginToSyntax
  , syntaxToBegin
  , beginToLetRec
    -- ** Optics
  , beginDefns
  , beginFinal
    -- * Define
  , Define (..)
    -- ** Basic Operations
  , defineToSyntax
    -- ** Optics
  , defineId
  , defineExpr
    -- * DefineSyntax
  , DefineSyntax (..)
    -- ** Basic Operations
  , defineSyntaxToSyntax
    -- ** Optics
  , defineSyntaxId
  , defineSyntaxExpr
  )
where

import Control.Lens (Lens', lens, (^.))

import Opal.Syntax (Identifier, Syntax, syntaxScope)
import Opal.Writer (Display (..), (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (id)
import Opal.Syntax.TH (syntax)
import Data.Default (Default(..))

-- Definition ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Definition
  = DefnDefine {-# UNPACK #-} !Define
    -- ^ A "define" definition.
  | DefnSyntax {-# UNPACK #-} !DefineSyntax
    -- ^ A "define-syntax" definition.
  | DefnExpr Syntax
    -- ^ An syntax object expression embedded into a definition context.
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display Definition where
  display (DefnDefine defn) = display defn
  display (DefnSyntax defn) = display defn
  display (DefnExpr   expr) = display expr

-- | @since 1.0.0
instance Show Definition where
  show (DefnDefine defn) = show defn
  show (DefnSyntax defn) = show defn
  show (DefnExpr   expr) = show expr

-- Definition - Basic Operations -----------------------------------------------

-- | Rebuild a definition as a raw syntax object.
--
-- @since 1.0.0
definitionToSyntax :: Definition -> Syntax
definitionToSyntax (DefnDefine defn) = defineToSyntax defn
definitionToSyntax (DefnSyntax defn) = defineSyntaxToSyntax defn
definitionToSyntax (DefnExpr   expr) = expr

-- Begin -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Begin = Begin
  { begin_defns :: [Definition]
    -- ^ A list of definitions or syntax expressions that make up the initial
    -- part of the 'SyntaxBody'.
  , begin_final  :: {-# UNPACK #-} !Syntax
    -- ^ The final syntax expression terminating the 'SyntaxBody'.
  }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display Begin where
  display x =
    mconcat
      [ Doc.string "(begin"
      , Doc.indent 2 (Doc.vsep (map display (beginToDefinitions x)))
      , Doc.char ')'
      ]

-- | @since 1.0.0
instance Show Begin where
  show = Doc.pretty . display

-- Begin - Basic Operations ----------------------------------------------------

-- | Convert a "begin" definition into a list of definitions. This is used to
-- flatten nested "begin" definitions into a single "begin".
--
-- @since 1.0.0
beginToDefinitions :: Begin -> [Definition]
beginToDefinitions (Begin defns final) = foldr (:) [DefnExpr final] defns

-- | Rebuild the given "define" definition as a raw syntax object.
--
-- @since 1.0.0
beginToSyntax :: Begin -> Syntax
beginToSyntax begin =
  let stxs = map definitionToSyntax (begin ^. beginDefns)
      stx  = begin ^. beginFinal
   in [syntax| (begin ?stxs ... ?stx) |]

-- | Wrap the given syntax object in a "begin" definition.
--
-- @since 1.0.0
syntaxToBegin :: Syntax -> Begin
syntaxToBegin = Begin []

-- | Convert a "begin" form into a "letrec-syntaxes+values" form.
--
-- @since 1.0.0
beginToLetRec :: Begin -> Syntax
beginToLetRec (Begin defns expr) =
  -- FIXME: Adding the core scope here seems dubious, but is necessary for the
  -- "letrec-syntaxes+values" identifier to resolve correctly.
  syntaxScope Nothing def [syntax|
    (letrec-syntaxes+values
      (?transIds ...)
      (?valIds ...)
      ?expr)
  |]
  where
    transIds :: [Syntax]
    transIds = foldr definitionToTransId [] defns

    definitionToTransId :: Definition -> [Syntax] -> [Syntax]
    definitionToTransId defn stxs = case defn of
      DefnSyntax (DefineSyntax id stx) -> [syntax| (?id:id ?stx) |] : stxs
      _                                -> stxs

    valIds :: [Syntax]
    valIds = foldr definitionToValId [] defns

    definitionToValId :: Definition -> [Syntax] -> [Syntax]
    definitionToValId defn stxs = case defn of
      DefnDefine (Define id stx) -> [syntax| (?id:id ?stx) |] : stxs
      DefnSyntax _               -> stxs
      DefnExpr   stx             -> [syntax| (_ ?stx)      |] : stxs

-- Begin - Optics --------------------------------------------------------------

-- | Lens focusing on the 'begin_defns' field of a 'Begin'.
--
-- @since 1.0.0
beginDefns :: Lens' Begin [Definition]
beginDefns = lens begin_defns \s x -> s { begin_defns = x }

-- | Lens focusing on the 'begin_final' field of a 'Begin'.
--
-- @since 1.0.0
beginFinal :: Lens' Begin Syntax
beginFinal = lens begin_final \s x -> s { begin_final = x }

-- Define ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Define = Define
  { define_id   :: {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  , define_expr :: Syntax
    -- ^ TODO: docs
  }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display Define where
  display (Define id defn) =
    Doc.string "(define"
      <+> display id
      <> Doc.nest 2 (Doc.line <> display defn)
      <> Doc.char ')'

-- | @since 1.0.0
instance Show Define where
  show (Define idt defn) = "(define " ++ show idt ++ show defn ++ ")"

-- Define - Basic Operations ---------------------------------------------------

-- | Rebuild the given "define" definition as a raw syntax object.
--
-- @since 1.0.0
defineToSyntax :: Define -> Syntax
defineToSyntax (Define id stx) =
  let coreId :: Syntax
      coreId = syntaxScope Nothing def [syntax| define |]
   in [syntax| (?coreId ?id:id ?stx) |]

-- Define - Optics -------------------------------------------------------------

-- | Lens focusing on the 'define_id' field of a 'Define'.
--
-- @since 1.0.0
defineId :: Lens' Define Identifier
defineId = lens define_id \s x -> s { define_id = x }

-- | Lens focusing on the 'define_expr' field of a 'Define'.
--
-- @since 1.0.0
defineExpr :: Lens' Define Syntax
defineExpr = lens define_expr \s x -> s { define_expr = x }

-- DefineSyntax ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DefineSyntax = DefineSyntax
  { define_syntax_id   :: {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  , define_syntax_expr :: Syntax
    -- ^ TODO: docs
  }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display DefineSyntax where
  display (DefineSyntax id defn) =
    Doc.string "(define-syntax"
      <+> display id
      <+> Doc.line
      <> Doc.nest 2 (display defn)
      <> Doc.char ')'

-- | @since 1.0.0
instance Show DefineSyntax where
  show (DefineSyntax idt defn) = "(define-syntax " ++ show idt ++ show defn ++ ")"

-- DefineSyntax - Basic Operations ---------------------------------------------

-- | Rebuild the given "define-syntax" definition as a raw syntax object.
--
-- @since 1.0.0
defineSyntaxToSyntax :: DefineSyntax -> Syntax
defineSyntaxToSyntax (DefineSyntax id stx) =
  let coreId :: Syntax
      coreId = syntaxScope Nothing def [syntax| define-syntax |]
   in [syntax| (?coreId ?id:id ?stx) |]

-- DefineSyntax - Optics -------------------------------------------------------

-- | Lens focusing on the 'define_syntax_id' field of a 'DefineSyntax'.
--
-- @since 1.0.0
defineSyntaxId :: Lens' DefineSyntax Identifier
defineSyntaxId = lens define_syntax_id \s x -> s { define_syntax_id = x }

-- | Lens focusing on the 'define_syntax_expr' field of a 'DefineSyntax'.
--
-- @since 1.0.0
defineSyntaxExpr :: Lens' DefineSyntax Syntax
defineSyntaxExpr = lens define_syntax_expr \s x -> s { define_syntax_expr = x }