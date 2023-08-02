{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax
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
module Opal.Syntax
  ( -- * Datum
    Datum (..)
    -- ** Basic Operations
  , datumKind
    -- * DatumKind
  , DatumKind (DatumKindId, ..)
    -- * Lambda
  , Lambda (..)
    -- ** Lenses
  , lambdaArgs
  , lambdaBody
    -- ** Query
  , lambdaArity
    -- * SExp
  , SExp (..)
    -- * Identifier
  , Identifier (..)
    -- ** Basic Operations
  , identifierToSyntax
    -- ** Scope Operations
  , identifierScope
    -- ** Lenses
  , idtSymbol
  , idtInfo
  , idtScopes
    -- * Syntax
  , Syntax (..)
    -- ** Basic Operations
  , datumToSyntax
  , syntaxToDatum
  , syntaxKind
    -- ** Scope Operations
  , syntaxScope
  , syntaxPrune
    -- ** Conversion
  , syntaxToIdentifier
    -- ** Lenses
  , stxDatum
  , stxInfo
  , stxScopes
  , stxProperties
    -- * SyntaxInfo
  , SyntaxInfo (..)
    -- ** Basic Operations
  , defaultSyntaxInfo
    -- ** Lenses
  , stxInfoSource
  , stxInfoSrcLoc
  , stxInfoProperties
  , stxInfoScopes
    -- * Transformer
  , Transformer (..)
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Lens', lens, view, (^.), over)

import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32)
import Data.IORef (IORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty

import GHC.Exts (IsList (..), IsString (..))
import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Phase (Phase)
import Opal.Common.Scope (Scope)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.SrcLoc (SrcLoc (..))
import Opal.Common.Symbol (Symbol, stringToSymbol, symbolToString)
import Opal.Writer.Class (Display(..))
import Opal.Writer.Doc qualified as Doc
import Opal.Syntax.ScopeInfo (ScopeInfo)
import Opal.Syntax.ScopeInfo qualified as ScopeInfo

import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

showBool :: Bool -> ShowS
showBool True  = showString "#t"
showBool False = showString "#f"

showSExp :: Show a => [a] -> ShowS
showSExp []       = showString "()"
showSExp (x : xs) = showChar '(' . shows x . run xs
  where
    run []       = showChar ')'
    run (y : ys) = showChar ' ' . shows y . run ys

-- Datum -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Datum
  = DatumB Bool
    -- ^ A literal boolean datum.
  | DatumC {-# UNPACK #-} !Char
    -- ^ A literal character datum.
  | DatumS {-# UNPACK #-} !Symbol
    -- ^ A literal symbol datum.
  | DatumI32 {-# UNPACK #-} !Int32
    -- ^ A literal 32-bit integer datum.
  | DatumF32 {-# UNPACK #-} !Float
    -- ^ A literal 32-bit floating-point number datum.
  | DatumLam {-# UNPACK #-} !Lambda
    -- ^ A 'Lambda' datum.
  | DatumList [Datum]
    -- ^ A list of datums.
  | DatumStx {-# UNPACK #-} !Syntax
    -- ^ The 'Datum' representation of a syntax object.
  deriving (Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Display Datum where
  display (DatumB    x) = if x then Doc.string "#t" else Doc.string "#f"
  display (DatumC    x) = Doc.string "#\\" <> Doc.char x
  display (DatumS    x) = display x
  display (DatumF32  x) = Doc.string (show x)
  display (DatumI32  x) = Doc.string (show x)
  display (DatumLam  x) = display x
  display (DatumList x) = displayList x
  display (DatumStx  x) = display x

  displayList xs = Doc.string "'(" <> Doc.sepMap display (Doc.char ' ') xs <> Doc.char ')'

-- | @since 1.0.0
instance NFData Datum

-- | @since 1.0.0
instance Show Datum where
  showsPrec _ (DatumB    x) = showBool x
  showsPrec _ (DatumC    x) = showString "#\\" . showChar x
  showsPrec p (DatumS    x) = showsPrec p x
  showsPrec p (DatumF32  x) = showsPrec p x
  showsPrec p (DatumI32  x) = showsPrec p x
  showsPrec p (DatumLam  x) = showsPrec p x
  showsPrec _ (DatumList x) = showList x
  showsPrec p (DatumStx  x) = showsPrec p x

  showList xs = showChar '\'' . showSExp xs

-- Datum - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
datumKind :: Datum -> DatumKind
datumKind DatumB    {}   = DatumKindBool
datumKind DatumC    {}   = DatumKindChar
datumKind DatumS    {}   = DatumKindSymbol
datumKind DatumF32  {}   = DatumKindF32
datumKind DatumI32  {}   = DatumKindI32
datumKind DatumLam  {}   = DatumKindLambda
datumKind DatumList {}   = DatumKindList
datumKind (DatumStx stx) = DatumKindSyntax (syntaxKind stx)

-- DatumKind -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DatumKind
  = DatumKindBool
    -- ^ TODO: docs
  | DatumKindChar
    -- ^ TODO: docs
  | DatumKindSymbol
    -- ^ TODO: docs
  | DatumKindF32
    -- ^ TODO: docs
  | DatumKindI32
    -- ^ TODO: docs
  | DatumKindLambda
    -- ^ TODO: docs
  | DatumKindList
    -- ^ TODO: docs
  | DatumKindSyntax DatumKind
    -- ^ TODO: docs
  deriving (Eq, Ord)

-- | TODO: docs
--
-- @since 1.0.0
pattern DatumKindId :: DatumKind
pattern DatumKindId = DatumKindSyntax DatumKindSymbol

-- | @since 1.0.0
instance Show DatumKind where
  show DatumKindBool       = "bool"
  show DatumKindChar       = "char"
  show DatumKindSymbol     = "symbol"
  show DatumKindF32        = "f32"
  show DatumKindI32        = "i32"
  show DatumKindLambda     = "lambda"
  show DatumKindList       = "list"
  show DatumKindId         = "id"
  show (DatumKindSyntax k) = "syntax-" ++ show k


-- Lambda ----------------------------------------------------------------------

-- | The 'Lambda' type represents a lambda or function value.
--
-- @since 1.0.0
data Lambda = Lambda
  { lambda_args :: [Symbol]
    -- ^ The arguments of the function.
  , lambda_body :: {-# UNPACK #-} !(NonEmpty SExp)
    -- ^ The body s-expressions of the function.
  }
  deriving (Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Display Lambda where
  display (Lambda args body) =
    Doc.vsep
      [ Doc.string "(lambda (" <> Doc.sepMap (Doc.string . symbolToString) (Doc.char ' ') (toList args) <> Doc.string ")"
      , Doc.nest 2 (displayList (NonEmpty.toList body) <> Doc.char ')')
      ]

-- | @since 1.0.0
instance NFData Lambda

-- | @since 1.0.0
instance Show Lambda where
  showsPrec p (Lambda args (x :| xs)) =
    showString "(lambda "
      . showSExp args
      . showChar ' '
      . showsPrec p x
      . showBody xs
    where
      showBody :: [SExp] -> ShowS
      showBody []       = showChar ')'
      showBody (y : ys) = showChar ' ' . showsPrec p y . showBody ys

-- Lambda - Lenses -------------------------------------------------------------

-- | Lens focusing on the 'lambda_args' field of 'Lambda'.
--
-- @since 1.0.0
lambdaArgs :: Lens' Lambda [Symbol]
lambdaArgs = lens lambda_args \s x -> s { lambda_args = x }
{-# INLINE lambdaArgs #-}

-- | Lens focusing on the 'lambda_body' field of 'Lambda'.
--
-- @since 1.0.0
lambdaBody :: Lens' Lambda (NonEmpty SExp)
lambdaBody = lens lambda_body \s x -> s { lambda_body = x }
{-# INLINE lambdaBody #-}

-- Lambda - Query --------------------------------------------------------------

-- | Obtain the number of arguments that the 'Lambda' accepts.
--
-- @since 1.0.0
lambdaArity :: Lambda -> Int
lambdaArity = length . view lambdaArgs

-- SExp ------------------------------------------------------------------------

-- | The 'SExp' type represents s-expressions.
--
-- @since 1.0.0
data SExp
  = SVal Datum
    -- ^ 'SVal' is a fully evaluated value.
  | SVar {-# UNPACK #-} !Symbol
    -- ^ 'SVar' is a reference to a variable.
  | SApp {-# UNPACK #-} !(NonEmpty SExp)
    -- ^ 'SApp' is an application. It is represented as a non-empty list of
    -- s-expressions, where the tail of the is list are applied to the first
    -- s-expression in the list.
  deriving (Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Display SExp where
  display (SVal val) = display val
  display (SVar var) = Doc.string (symbolToString var)
  display (SApp exs) = displayList (NonEmpty.toList exs)

  displayList xs = Doc.string "(" <> Doc.sepMap display (Doc.char ' ') xs <> Doc.char ')'

-- | @since 1.0.0
instance IsList SExp where
  type Item SExp = SExp

  fromList []         = errorWithoutStackTrace "(fromList @SExp []): empty lists cannot be converted to s-expressions"
  fromList (ex : exs) = SApp (ex :| exs)

  toList (SApp sexps) = NonEmpty.toList sexps
  toList other        = errorWithoutStackTrace ("(toList @SExp): s-expression is not a list: " ++ show other)

-- | @since 1.0.0
instance IsString SExp where
  fromString = SVar . stringToSymbol

-- | @since 1.0.0
instance NFData SExp

-- | @since 1.0.0
instance Show SExp where
  showsPrec p (SVal val) = showsPrec p val
  showsPrec p (SVar var) = showsPrec p var
  showsPrec _ (SApp exs) = showList (NonEmpty.toList exs)

  showList = showSExp

-- SExp - Basic Operations -----------------------------------------------------


-- Identifier ------------------------------------------------------------------

-- | The 'Identifier' type represents a 'Symbol' with lexical information. Any
-- syntax object wrapping a 'Symbol' is an 'Identifier'. The 'Identifier' type
-- can be directly converted to a syntax object via 'identifierToSyntax'.
--
-- @since 1.0.0
data Identifier = Identifier
  { idt_symbol :: {-# UNPACK #-} !Symbol
    -- ^ The identifier's symbol.
  , idt_info   :: {-# UNPACK #-} !SyntaxInfo
    -- ^ The lexical information associated with the identifier.
  }
  deriving (Eq, Generic, Ord)

-- | @since 1.0.0
instance Display Identifier where
  display (Identifier s _) = Doc.char '#' <> display s

-- | @since 1.0.0
instance Show Identifier where
  showsPrec p (Identifier s _) = showString "#" . showsPrec p s

-- Identifier - Basic Operations -----------------------------------------------

-- | Convert 'Identifier' to a syntax object.
--
-- @since 1.0.0
identifierToSyntax :: Identifier -> Syntax
identifierToSyntax (Identifier sym info) = Syntax (DatumS sym) info

-- Identifier - Scope Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
identifierScope :: Maybe Phase -> Scope -> Identifier -> Identifier
identifierScope ph sc = over idtScopes (ScopeInfo.insert ph sc)

-- Identifier - Lenses ---------------------------------------------------------

-- | Lens focusing on the 'idt_symbol' field of 'Identifier'.
--
-- @since 1.0.0
idtSymbol :: Lens' Identifier Symbol
idtSymbol = lens idt_symbol \s x -> s { idt_symbol = x }
{-# INLINE idtSymbol #-}

-- | Lens focusing on the 'idt_info' field of 'Identifier'.
--
-- @since 1.0.0
idtInfo :: Lens' Identifier SyntaxInfo
idtInfo = lens idt_info \s x -> s { idt_info = x }
{-# INLINE idtInfo #-}

-- | Compound lens focusing on @('idtInfo' . 'stxInfoScopes')@ field of an
-- 'Identifier'.
--
-- @since 1.0.0
idtScopes :: Lens' Identifier ScopeInfo
idtScopes = idtInfo . stxInfoScopes
{-# INLINE idtScopes #-}

-- Syntax ----------------------------------------------------------------------

-- | The 'Syntax' record represents syntax objects. A syntax object is a datum
-- equipped with lexical information.
--
-- @since 1.0.0
data Syntax = Syntax
  { stx_datum :: Datum
    -- ^ The datum that the wrapped by the syntax object.
  , stx_info  :: {-# UNPACK #-} !SyntaxInfo
    -- ^ The lexical information associated with the syntax object.
  }
  deriving (Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Display Syntax where
  display stx = case stx ^. stxDatum of
    DatumS    x -> Doc.char '#' <> display x
    DatumList x -> Doc.group (Doc.char '#' <> displayList x)
    datum       -> display datum

-- | @since 1.0.0
instance IsList Syntax where
  type Item Syntax = Syntax

  fromList stxs = Syntax (DatumList (map DatumStx stxs)) def

  toList (Syntax (DatumList stxs) info) = map (datumToSyntax info) stxs
  toList _ = errorWithoutStackTrace "(toList @Syntax): syntax object is not a list"

-- | @since 1.0.0
instance NFData Syntax

-- | @since 1.0.0
instance Show Syntax where
  showsPrec p (Syntax (DatumS s)       _) = showChar '#' . showsPrec p s
  showsPrec p (Syntax (DatumList stxs) _) = showChar '#' . showsPrec p stxs
  showsPrec p (Syntax val              _) = showsPrec p val

  showList xs = showString "#'" . showSExp xs

-- Syntax - Basic Operations ---------------------------------------------------

-- | @(datumToSyntax ctxt v)@ converts the given 'Datum') @v@ to a 'Syntax'
-- object.
--
--   * If @v@ is already a syntax object, then there is no conversion, and @v@
--     is returned unmodified.
--
--   * If @v@ is a list, then the contents of @v@ are recursively converted to
--     syntax objects.
--
--   * For any other kind of 'Datum', conversion means wrapping @v@ with the
--     lexical information @ctxt@.
--
-- @since 1.0.0
datumToSyntax :: SyntaxInfo -> Datum -> Syntax
datumToSyntax _    (DatumStx  stx) = stx
datumToSyntax ctxt (DatumList vxs) = Syntax (DatumList (map (DatumStx . datumToSyntax ctxt) vxs)) ctxt
datumToSyntax ctxt datum           = Syntax datum ctxt

-- | Converts a 'Syntax' object to a datum by stripping the syntax object's
-- lexical information.
--
-- @since 1.0.0
syntaxToDatum :: Syntax -> Datum
syntaxToDatum stx = case stx ^. stxDatum of
  DatumStx  v   -> syntaxToDatum v
  DatumList vxs -> DatumList (map stripSyntax vxs)
  datum         -> datum
  where
    stripSyntax :: Datum -> Datum
    stripSyntax (DatumStx v) = syntaxToDatum v
    stripSyntax datum        = datum

-- | TODO: docs
--
-- @since 1.0.0
syntaxKind :: Syntax -> DatumKind
syntaxKind = datumKind . syntaxToDatum

-- Syntax - Scope Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntaxScope :: Maybe Phase -> Scope -> Syntax -> Syntax
syntaxScope ph sc stx =
  let val  = datumScope (syntaxToDatum stx)
      info = over stxInfoScopes (ScopeInfo.insert ph sc) (stx ^. stxInfo)
   in Syntax val info
  where
    datumScope :: Datum -> Datum
    datumScope (DatumStx  val)  = DatumStx (syntaxScope ph sc val)
    datumScope (DatumList vals) = DatumList (map datumScope vals)
    datumScope val              = val

-- | TODO: docs
--
-- @since 1.0.0
syntaxPrune :: Phase -> ScopeSet -> Syntax -> Syntax
syntaxPrune ph scps stx =
  let val  = datumPrune (syntaxToDatum stx)
      info = over stxInfoScopes (ScopeInfo.deletes ph scps) (stx ^. stxInfo)
   in Syntax val info
  where
    datumPrune :: Datum -> Datum
    datumPrune (DatumStx  val)  = DatumStx (syntaxPrune ph scps val)
    datumPrune (DatumList vals) = DatumList (map datumPrune vals)
    datumPrune val              = val

-- Syntax - Conversion ---------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntaxToIdentifier :: Syntax -> Maybe Identifier
syntaxToIdentifier stx = case syntaxToDatum stx of
  DatumS s -> Just (Identifier s (stx ^. stxInfo))
  _        -> Nothing

-- Syntax - Lenses -------------------------------------------------------------

-- | Lens focusing on the 'stx_datum' field of 'Syntax'.
--
-- @since 1.0.0
stxDatum :: Lens' Syntax Datum
stxDatum = lens stx_datum \s x -> s { stx_datum = x }
{-# INLINE stxDatum #-}

-- | Lens focusing on the 'stx_info' field of 'Syntax'.
--
-- @since 1.0.0
stxInfo :: Lens' Syntax SyntaxInfo
stxInfo = lens stx_info \s x -> s { stx_info = x }
{-# INLINE stxInfo #-}

-- | Compound lens focusing on @('stxInfo' . 'stxInfoProperties')@ field of a
-- 'Syntax'.
--
-- @since 1.0.0
stxProperties :: Lens' Syntax (HashMap Symbol Syntax)
stxProperties = stxInfo . stxInfoProperties
{-# INLINE stxProperties #-}

-- | Compound lens focusing on @('stxInfo' . 'stxInfoScopes')@ field of a
-- 'Syntax'.
--
-- @since 1.0.0
stxScopes :: Lens' Syntax ScopeInfo
stxScopes = stxInfo . stxInfoScopes
{-# INLINE stxScopes #-}

-- SyntaxInfo ------------------------------------------------------------------

-- | 'SyntaxInfo' is a record containing lexical information that is associated
-- with syntax objects such as 'Syntax' or 'Identifier'.
--
-- @since 1.0.0
data SyntaxInfo = SyntaxInfo
  { stx_info_source :: Maybe FilePath
    -- ^ An optional path to the source file that the associated syntax object
    -- originated from.
  , stx_info_srcloc :: Maybe SrcLoc
    -- ^ An optional source location that the associated syntax object
    -- originated from.
  , stx_info_scopes :: {-# UNPACK #-} !ScopeInfo
    -- ^ The set of phase-specific scopes and global scopes that are attached to
    -- the syntax object associated with the 'SyntaxInfo'.
  , stx_info_properties :: HashMap Symbol Syntax
    -- ^ The syntax properties map.
  }
  deriving (Eq, Generic, Lift, Ord, Show)

-- | 'SyntaxInfo' defaults to 'defaultSyntaxInfo'.
--
-- @since 1.0.0
instance Default SyntaxInfo where
  def = defaultSyntaxInfo

-- | @since 1.0.0
instance NFData SyntaxInfo

-- SyntaxInfo - Basic Operations -----------------------------------------------

-- | The default lexical information.
--
-- >>> defaultSyntaxInfo
-- SyntaxInfo
--   { stx_info_source = Nothing
--   , stx_info_srcloc = Nothing
--   , stx_info_scopes = ScopeInfo (fromList []) (fromList [])
--   }
--
-- @since 1.0.0
defaultSyntaxInfo :: SyntaxInfo
defaultSyntaxInfo = SyntaxInfo def def def HashMap.empty

-- SyntaxInfo - Lenses ---------------------------------------------------------

-- | Lens focusing on the 'stx_info_properties' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoSource :: Lens' SyntaxInfo (Maybe FilePath)
stxInfoSource = lens stx_info_source \s x -> s { stx_info_source = x }
{-# INLINE stxInfoSource #-}

-- | Lens focusing on the 'stx_info_srcloc' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoSrcLoc :: Lens' SyntaxInfo (Maybe SrcLoc)
stxInfoSrcLoc = lens stx_info_srcloc \s x -> s { stx_info_srcloc = x }
{-# INLINE stxInfoSrcLoc #-}

-- | Lens focusing on the 'stx_info_properties' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoProperties :: Lens' SyntaxInfo (HashMap Symbol Syntax)
stxInfoProperties = lens stx_info_properties \s x -> s { stx_info_properties = x }
{-# INLINE stxInfoProperties #-}

-- | Lens focusing on the 'stx_info_srcloc' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoScopes :: Lens' SyntaxInfo ScopeInfo
stxInfoScopes = lens stx_info_scopes \s x -> s { stx_info_scopes = x }
{-# INLINE stxInfoScopes #-}

-- Transformer -----------------------------------------------------------------

-- | 'Transformer' is the type used to represent compile-time "meanings".
--
-- @since 1.0.0
data Transformer
  = TransformerLambda
    -- ^ 'TransformerLambda' is used to represent the core syntactic form for
    -- @lambda@.
  | TransformerLetSyntax
    -- ^ 'TransformerLambda' is used to represent the core syntactic form for
    -- @let-syntax@.
  | TransformerQuote
    -- ^ 'TransformerLambda' is used to represent the core syntactic form for
    -- @quote@.
  | TransformerQuoteSyntax
    -- ^ 'TransformerLambda' is used to represent the core syntactic form for
    -- @quote-syntax@.
  | TransformerVar {-# UNPACK #-} !Identifier
    -- ^ 'TransformerVar' is a reference to a function argument.
  | TransformerVal (IORef Datum)
    -- ^ 'TransformerVal' is a compile-time value. In the special case that the
    -- compile-time value is a function, then that function is a macro
    -- transformer.

-- | @since 1.0.0
instance Show Transformer where
  showsPrec _ TransformerLambda       = showString "#<transformer-lambda>"
  showsPrec _ TransformerLetSyntax    = showString "#<transformer-let-syntax>"
  showsPrec _ TransformerQuote        = showString "#<transformer-quote>"
  showsPrec _ TransformerQuoteSyntax  = showString "#<transformer-quote-syntax>"
  showsPrec p (TransformerVar var)    =
    showString "#<transformer-var "
      . showsPrec p var
      . showChar '>'
  showsPrec p (TransformerVal datum)  =
    showString "#<transformer-val "
      . showsPrec p (unsafePerformIO (readIORef datum))
      . showChar '>'
