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
  ( -- * SyntaxBody
    SyntaxBody (..)
    -- ** Basic Operations
  , syntaxToSyntaxBody
  , syntaxBodyToDefinitions
    -- ** Optics
  , syntaxBodyDefns
  , syntaxBodyFinal
    -- * Definition
  , Definition (..)
    -- ** Basic Operations
  , definitionToSyntax
    -- * Begin
  , Begin (..)
    -- ** Basic Operations
  , beginToDefinitions
  , beginToSyntax
  , syntaxToBegin
    -- ** Optics
  , beginBody
  , beginDefns
  , beginFinal
    -- * Define
  , Define (..)
    -- ** Basic Operations
  , defineToSyntax
    -- ** Optics
  , defineId
  , defineDefn
    -- * DefineSyntax
  , DefineSyntax (..)
    -- ** Basic Operations
  , defineSyntaxToSyntax
    -- ** Optics
  , defineSyntaxId
  , defineSyntaxStx
  )
where

import Control.Lens (Lens', lens, (^.))

import Data.Coerce (coerce)

import Opal.Syntax (Identifier, Syntax)
import Opal.Writer (Display (..), (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (id)
import Opal.Syntax.TH (syntax)

-- SyntaxBody ------------------------------------------------------------------

-- | A 'SyntaxBody' is the body of a "begin" definition. It is a list of
-- definitions or syntax objects terminated by a syntax object.
--
-- @since 1.0.0
data SyntaxBody = SyntaxBody
  { syntax_body_defns :: [Definition]
    -- ^ A list of definitions or syntax expressions that make up the initial
    -- part of the 'SyntaxBody'.
  , syntax_body_final :: Syntax
    -- ^ The final syntax expression terminating the 'SyntaxBody'.
  }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display SyntaxBody where
  display (SyntaxBody []    final) = display final
  display (SyntaxBody defns final) = foldr ((<+>) . display) (display final) defns

-- | @since 1.0.0
instance Show SyntaxBody where
  show (SyntaxBody []    final) = show final
  show (SyntaxBody defns final) = foldr ((++) . (:) ' ' . show) (show final) defns

-- SyntaxBody - Basic Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntaxToSyntaxBody :: Syntax -> SyntaxBody
syntaxToSyntaxBody = SyntaxBody []

-- | TODO: docs
--
-- @since 1.0.0
syntaxBodyToDefinitions :: SyntaxBody -> [Definition]
syntaxBodyToDefinitions (SyntaxBody defns stx) = foldr (:) [DefnSyntax stx] defns

-- SyntaxBody - Optics ---------------------------------------------------------

-- | Lens focusing on the 'syntax_body_defns' field of a 'SyntaxBody'.
--
-- @since 1.0.0
syntaxBodyDefns :: Lens' SyntaxBody [Definition]
syntaxBodyDefns = lens syntax_body_defns \s x -> s { syntax_body_defns = x }

-- | Lens focusing on the 'syntax_body_final' field of a 'SyntaxBody'.
--
-- @since 1.0.0
syntaxBodyFinal :: Lens' SyntaxBody Syntax
syntaxBodyFinal = lens syntax_body_final \s x -> s { syntax_body_final = x }

-- Definition ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Definition
  = DefnBegin  {-# UNPACK #-} !Begin
    -- ^ A "begin" definition.
  | DefnDefine {-# UNPACK #-} !Define
    -- ^ A "define" definition.
  | DefnDefineSyntax {-# UNPACK #-} !DefineSyntax
    -- ^ A "define-syntax" definition.
  | DefnSyntax  Syntax
    -- ^ An syntax object expression embedded into a definition context.
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display Definition where
  display (DefnBegin        defn) = display defn
  display (DefnDefine       defn) = display defn
  display (DefnDefineSyntax defn) = display defn
  display (DefnSyntax       stx)  = display stx

-- | @since 1.0.0
instance Show Definition where
  show (DefnBegin        defn) = show defn
  show (DefnDefine       defn) = show defn
  show (DefnDefineSyntax defn) = show defn
  show (DefnSyntax       stx)  = show stx

-- Definition - Basic Operations -----------------------------------------------

-- | Rebuild a definition as a raw syntax object.
--
-- @since 1.0.0
definitionToSyntax :: Definition -> Syntax
definitionToSyntax (DefnBegin        defn) = beginToSyntax defn
definitionToSyntax (DefnDefine       defn) = defineToSyntax defn
definitionToSyntax (DefnDefineSyntax defn) = defineSyntaxToSyntax defn
definitionToSyntax (DefnSyntax       stx)  = stx

-- Begin -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Begin = Begin
  { getSyntaxBegin :: SyntaxBody }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display Begin where
  display (Begin body) = Doc.string "(begin" <+> display body <> Doc.char ')'

-- | @since 1.0.0
instance Show Begin where
  show (Begin body) = "(begin " ++ show body ++ ")"

-- Begin - Basic Operations ----------------------------------------------------

-- | Convert a "begin" definition into a list of definitions. This is used to
-- flatten nested "begin" definitions into a single "begin".
--
-- @since 1.0.0
beginToDefinitions :: Begin -> [Definition]
beginToDefinitions = syntaxBodyToDefinitions . getSyntaxBegin

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
syntaxToBegin = Begin . syntaxToSyntaxBody

-- Begin - Optics --------------------------------------------------------------

-- | Lens focusing on the 'getSyntaxBegin' field of a 'Begin'.
--
-- @since 1.0.0
beginBody :: Lens' Begin SyntaxBody
beginBody = lens coerce \_ x -> coerce x

-- | Compound lens focusing on the @('syntaxBeginBody' . 'syntaxBodyDefns')@
-- field of a 'Begin'.
--
-- @since 1.0.0
beginDefns :: Lens' Begin [Definition]
beginDefns = beginBody . syntaxBodyDefns

-- | Compound lens focusing on the @('syntaxBeginBody' . 'syntaxBodyFinal')@
-- field of a 'Begin'.
--
-- @since 1.0.0
beginFinal :: Lens' Begin Syntax
beginFinal = beginBody . syntaxBodyFinal

-- Define ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Define = Define
  { define_id   :: {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  , define_defn :: Syntax
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
defineToSyntax (Define id stx) = [syntax| (define ?id:id ?stx) |]

-- Define - Optics -------------------------------------------------------------

-- | Lens focusing on the 'define_id' field of a 'Define'.
--
-- @since 1.0.0
defineId :: Lens' Define Identifier
defineId = lens define_id \s x -> s { define_id = x }

-- | Lens focusing on the 'define_defn' field of a 'Define'.
--
-- @since 1.0.0
defineDefn :: Lens' Define Syntax
defineDefn = lens define_defn \s x -> s { define_defn = x }

-- DefineSyntax ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DefineSyntax = DefineSyntax
  { define_syntax_id  :: {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  , define_syntax_stx :: Syntax
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

-- BeDefineSyntaxgin - Basic Operations ----------------------------------------

-- | Rebuild the given "define-syntax" definition as a raw syntax object.
--
-- @since 1.0.0
defineSyntaxToSyntax :: DefineSyntax -> Syntax
defineSyntaxToSyntax (DefineSyntax id stx) = [syntax| (define-syntax ?id:id ?stx) |]

-- DefineSyntax - Optics -------------------------------------------------------

-- | Lens focusing on the 'define_syntax_id' field of a 'DefineSyntax'.
--
-- @since 1.0.0
defineSyntaxId :: Lens' DefineSyntax Identifier
defineSyntaxId = lens define_syntax_id \s x -> s { define_syntax_id = x }

-- | Lens focusing on the 'define_syntax_stx' field of a 'DefineSyntax'.
--
-- @since 1.0.0
defineSyntaxStx :: Lens' DefineSyntax Syntax
defineSyntaxStx = lens define_syntax_stx \s x -> s { define_syntax_stx = x }