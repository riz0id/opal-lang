{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Parse.ParseError 
  ( -- * ParseError 
    ParseError (..),

    -- ** Doc 
    pprParseError,
    docParseError,
  
    -- * CoreParseError 
    CoreParseError (..),

    -- ** Doc 
    docCoreParseError,
  )
where 

import Control.Exception (Exception)

import Data.Text (Text)

import Text.Emit (Doc)
import qualified Text.Emit as Emit

--------------------------------------------------------------------------------

import Opal.Core.Form (CoreForm)
import Opal.Core.Datum (Datum)
import qualified Opal.Core.Form as Core.Form

import Opal.Expand.Syntax (Syntax)

import qualified Opal.Print as Print

-- ParseError ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseError 
  = ExnParseCore {-# UNPACK #-} !CoreParseError
  | ExnMissingProc {-# UNPACK #-} !Syntax
  | -- | TODO
    ExnParseCase [Syntax]
  | -- | TODO
    ExnParseClause Syntax
  | -- | TODO 
    ExnParseIf Syntax [Syntax]
  | -- | TODO 
    ExnParseModule Syntax
  | -- | TODO
    ExnParseLetBind Syntax
  | -- | TODO
    ExnParseLetSyntax [Syntax]
  | -- | TODO
    ExnParseLetSyntaxBind Syntax
  | -- | TODO
    ExnParseSyntax [Syntax]
  | -- | TODO
    ExnParseQuasiSyntax [Syntax]
  | -- | TODO
    ExnParseStxIdt Syntax
  | -- | TODO 
    ExnParseDefineValue [Syntax]
  | -- | TODO
    ExnParseQuote [Syntax]
  deriving (Eq, Exception, Ord, Show)

-- ParseError - Doc ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pprParseError :: ParseError -> Text
pprParseError exn = Emit.layout (docParseError exn)

-- | TODO
--
-- @since 1.0.0
docParseError :: ParseError -> Doc a 
docParseError (ExnParseCore exn) = docCoreParseError exn
docParseError other = Emit.string (show other)

-- CoreParseError --------------------------------------------------------------

-- | 'CoreParseError' is a parser error emitted when a core syntactic form 
-- cannot be parsed.
--
-- @since 1.0.0
data CoreParseError = CoreParseError 
  { exnForm :: CoreForm
    -- ^ 'exnForm' is the core form the parser was attempting to parse when this
    -- 'CoreParseError' was raised. 
  , exnSyntax :: {-# UNPACK #-} !Syntax
    -- ^ 'exnSyntax' is the full syntax object that the parser rejected. 
  , exnOrigin :: Maybe Datum 
    -- ^ 'exnOrigin' is either a datum within 'exnSyntax', or 'Nothing'. If
    -- given, 'exnSyntax' is used to refine the rejected syntax to a particular
    -- form within 'exnSyntax'.
  }
  deriving (Eq, Exception, Ord, Show)

-- CoreParseError - Doc --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docCoreParseError :: CoreParseError -> Doc a 
docCoreParseError (CoreParseError form stx orig) = 
  docExnForm <> docExnSyntax
  where 
    docExnForm :: Doc a 
    docExnForm = 
      Emit.hsep 
        [ "could not parse core syntax"
        , "'" <> Core.Form.docCoreForm form <> "'"
        ]

    docExnSyntax :: Doc a 
    docExnSyntax = case orig of 
      Nothing -> 
        (Emit.nest 2 . Emit.vsep)
          [ Emit.hsep 
              [ "* for the syntax:"
              , Print.docSyntax stx
              ]
          ]
      Just origin -> 
        (Emit.nest 2 . Emit.vsep)
          [ "* for the s-expression:"
          , Print.docDatum origin
          , "* within the syntax:"
          , Print.docSyntax stx
          ]