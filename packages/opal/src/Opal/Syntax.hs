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
  ( -- * Value
    Value (..)
    -- ** Optics
  , valueBool
  , valueChar
  , valueSymbol
  , valueF32
  , valueI32
  , valueLambda
    -- * Datum
  , Datum (DatumB, DatumC, DatumS, DatumF32, DatumI32, DatumLam, ..)
    -- ** Optics
  , datumValue
  , datumBool
  , datumChar
  , datumSymbol
  , datumF32
  , datumI32
  , datumLambda
  , datumList
  , datumSyntax
    -- * Lambda
  , Lambda (..)
    -- ** Optics
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
    -- ** Optics
  , idtSymbol
  , idtInfo
  , idtScopes
    -- * Syntax
  , Syntax (SyntaxB, SyntaxC, SyntaxS, SyntaxF32, SyntaxI32, SyntaxLam, ..)
    -- ** Basic Operations
  , datumToSyntax
  , syntaxToDatum
    -- ** Scope Operations
  , syntaxScope
  , syntaxPrune
    -- ** Optics
  , syntaxDatum
  , syntaxInfo
  , syntaxScopes
  , syntaxProperties
  , syntaxBool
  , syntaxChar
  , syntaxF32
  , syntaxI32
  , syntaxId
  , syntaxList
    -- * SyntaxInfo
  , SyntaxInfo (..)
    -- ** Basic Operations
  , defaultSyntaxInfo
    -- ** Optics
  , stxInfoSource
  , stxInfoScopes
  , stxInfoProperties
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Lens', Prism', lens, over, preview, prism', view, (^.))

import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty

import GHC.Exts (IsList (..), IsString (..))
import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Phase (Phase)
import Opal.Common.Scope (Scope)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.SourceInfo (SourceInfo)
import Opal.Common.Symbol (Symbol, stringToSymbol, symbolToString)
import Opal.Writer (Display(..))
import Opal.Writer qualified as Doc
import Opal.Syntax.ScopeInfo (ScopeInfo)
import Opal.Syntax.ScopeInfo qualified as ScopeInfo

import Prelude hiding (id)

-- Value -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Value
  = ValueB   !Bool
    -- ^ A literal boolean value.
  | ValueC   {-# UNPACK #-} !Char
    -- ^ A literal character value.
  | ValueS   {-# UNPACK #-} !Symbol
    -- ^ A literal symbol value.
  | ValueF32 {-# UNPACK #-} !Float
    -- ^ A literal 32-bit floating point number value.
  | ValueI32 {-# UNPACK #-} !Int32
    -- ^ A literal 32-bit integer value.
  | ValueLam {-# UNPACK #-} !Lambda
    -- ^ A literal function value.
  deriving (Eq, Generic, Ord, Lift)

-- | @since 1.0.0
instance Display Value where
  display (ValueB    x) = if x then Doc.string "#t" else Doc.string "#f"
  display (ValueC    x) = Doc.string "#\\" <> Doc.char x
  display (ValueS    x) = display x
  display (ValueF32  x) = Doc.string (show x)
  display (ValueI32  x) = Doc.string (show x)
  display (ValueLam  x) = Doc.string (show x)

-- | @since 1.0.0
instance NFData Value

-- | @since 1.0.0
instance Show Value where
  show = Doc.pretty 80 . display

-- Datum - Optics --------------------------------------------------------------

-- | Prism focusing on the 'ValueB' constructor of 'Datum'.
--
-- @since 1.0.0
valueBool :: Prism' Value Bool
valueBool = prism' ValueB \case { ValueB x -> Just x; _ -> Nothing }
{-# INLINE valueBool #-}

-- | Prism focusing on the 'ValueC' constructor of 'Datum'.
--
-- @since 1.0.0
valueChar :: Prism' Value Char
valueChar = prism' ValueC \case { ValueC x -> Just x; _ -> Nothing }
{-# INLINE valueChar #-}

-- | Prism focusing on the 'ValueS' constructor of 'Datum'.
--
-- @since 1.0.0
valueSymbol :: Prism' Value Symbol
valueSymbol = prism' ValueS \case { ValueS x -> Just x; _ -> Nothing }
{-# INLINE valueSymbol #-}

-- | Prism focusing on the 'ValueF32' constructor of 'Datum'.
--
-- @since 1.0.0
valueF32 :: Prism' Value Float
valueF32 = prism' ValueF32 \case { ValueF32 x -> Just x; _ -> Nothing }
{-# INLINE valueF32 #-}

-- | Prism focusing on the 'DatumI32' constructor of 'Datum'.
--
-- @since 1.0.0
valueI32 :: Prism' Value Int32
valueI32 = prism' ValueI32 \case { ValueI32 x -> Just x; _ -> Nothing }
{-# INLINE valueI32 #-}

-- | Prism focusing on the 'ValueLam' constructor of 'Datum'.
--
-- @since 1.0.0
valueLambda :: Prism' Value Lambda
valueLambda = prism' ValueLam \case { ValueLam x -> Just x; _ -> Nothing }
{-# INLINE valueLambda #-}

-- Datum -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Datum
  = DatumVal Value
    -- ^ TODO: docs
  | DatumList [Datum]
    -- ^ A list of datums.
  | DatumStx {-# UNPACK #-} !Syntax
    -- ^ The 'Datum' representation of a syntax object.
  deriving (Eq, Generic, Lift, Ord)

-- | Pattern synonym for @('DatumVal' (ValueB _) _)@.
--
-- @since 1.0.0
pattern DatumB :: Bool -> Datum
pattern DatumB x = DatumVal (ValueB x)

-- | Pattern synonym for @('DatumVal' (ValueC _) _)@.
--
-- @since 1.0.0
pattern DatumC :: Char -> Datum
pattern DatumC x = DatumVal (ValueC x)

-- | Pattern synonym for @('DatumVal' (ValueS _) _)@.
--
-- @since 1.0.0
pattern DatumS :: Symbol -> Datum
pattern DatumS x = DatumVal (ValueS x)

-- | Pattern synonym for @('DatumVal' (ValueF32 _) _)@.
--
-- @since 1.0.0
pattern DatumF32 :: Float -> Datum
pattern DatumF32 x = DatumVal (ValueF32 x)

-- | Pattern synonym for @('DatumVal' (ValueI32 _) _)@.
--
-- @since 1.0.0
pattern DatumI32 :: Int32 -> Datum
pattern DatumI32 x = DatumVal (ValueI32 x)

-- | Pattern synonym for @('DatumVal' (ValueLam _) _)@.
--
-- @since 1.0.0
pattern DatumLam :: Lambda -> Datum
pattern DatumLam x = DatumVal (ValueLam x)

{-# COMPLETE
  DatumB, DatumC, DatumS, DatumF32, DatumI32, DatumLam, DatumList, DatumStx
  #-}

-- | @since 1.0.0
instance Display Datum where
  display (DatumVal    x) = display x
  display (DatumLam  x) = display x
  display (DatumList x) = displayList x
  display (DatumStx  x) = display x

  displayList xs = Doc.string "'(" <> Doc.sepMap display (Doc.char ' ') xs <> Doc.char ')'

-- | @since 1.0.0
instance NFData Datum

-- | @since 1.0.0
instance Show Datum where
  show = Doc.pretty 80 . display

-- Datum - Optics --------------------------------------------------------------

-- | Prism focusing on the 'DatumVal' constructor of 'Datum'.
--
-- @since 1.0.0
datumValue :: Prism' Datum Value
datumValue = prism' DatumVal \case { DatumVal x -> Just x; _ -> Nothing }
{-# INLINE datumValue #-}

-- | Compound prism focusing on the @('datumValue' . 'datumBool')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
datumBool :: Prism' Datum Bool
datumBool = datumValue . valueBool
{-# INLINE datumBool #-}

-- | Compound prism focusing on the @('datumValue' . 'valueChar')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
datumChar :: Prism' Datum Char
datumChar = datumValue . valueChar
{-# INLINE datumChar #-}

-- | Compound prism focusing on the @('datumValue' . 'valueSymbol')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
datumSymbol :: Prism' Datum Symbol
datumSymbol = datumValue . valueSymbol

-- | Compound prism focusing on the @('datumValue' . 'valueF32')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
datumF32 :: Prism' Datum Float
datumF32 = datumValue . valueF32

-- | Compound prism focusing on the @('datumValue' . 'valueI32')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
datumI32 :: Prism' Datum Int32
datumI32 = datumValue . valueI32

-- | Prism focusing on the 'DatumLam' constructor of 'Datum'.
--
-- @since 1.0.0
datumLambda :: Prism' Datum Lambda
datumLambda = prism' DatumLam \case { DatumLam x -> Just x; _ -> Nothing }

-- | Prism focusing on the 'DatumList' constructor of 'Datum'.
--
-- @since 1.0.0
datumList :: Prism' Datum [Datum]
datumList = prism' DatumList \case { DatumList x -> Just x; _ -> Nothing }

-- | Prism focusing on the 'DatumStx' constructor of 'Datum'.
--
-- @since 1.0.0
datumSyntax :: Prism' Datum Syntax
datumSyntax = prism' DatumStx \case { DatumStx x -> Just x; _ -> Nothing }

-- Lambda ----------------------------------------------------------------------

-- | The 'Lambda' type represents a lambda or function value.
--
-- @since 1.0.0
data Lambda = Lambda
  { lambda_args :: [Symbol]
    -- ^ The arguments of the function.
  , lambda_body :: {-# UNPACK #-} !SExp
    -- ^ The body s-expressions of the function.
  }
  deriving (Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Display Lambda where
  display (Lambda args body) =
    Doc.vsep
      [ Doc.string "(lambda (" <> Doc.sepMap (Doc.string . symbolToString) (Doc.char ' ') (toList args) <> Doc.string ")"
      , Doc.nest 2 (display body <> Doc.char ')')
      ]

-- | @since 1.0.0
instance NFData Lambda

-- | @since 1.0.0
instance Show Lambda where
  show = Doc.pretty 80 . display

-- Lambda - Optics -------------------------------------------------------------

-- | Lens focusing on the 'lambda_args' field of 'Lambda'.
--
-- @since 1.0.0
lambdaArgs :: Lens' Lambda [Symbol]
lambdaArgs = lens lambda_args \s x -> s { lambda_args = x }

-- | Lens focusing on the 'lambda_body' field of 'Lambda'.
--
-- @since 1.0.0
lambdaBody :: Lens' Lambda SExp
lambdaBody = lens lambda_body \s x -> s { lambda_body = x }

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
  show = Doc.pretty 80 . display

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
  show = Doc.pretty 80 . display

-- Identifier - Basic Operations -----------------------------------------------

-- | Convert 'Identifier' to a syntax object.
--
-- @since 1.0.0
identifierToSyntax :: Identifier -> Syntax
identifierToSyntax (Identifier s info) = SyntaxS s info

-- Identifier - Scope Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
identifierScope :: Maybe Phase -> Scope -> Identifier -> Identifier
identifierScope ph sc = over idtScopes (ScopeInfo.insert ph sc)

-- Identifier - Optics ---------------------------------------------------------

-- | Lens focusing on the 'idt_symbol' field of 'Identifier'.
--
-- @since 1.0.0
idtSymbol :: Lens' Identifier Symbol
idtSymbol = lens idt_symbol \s x -> s { idt_symbol = x }

-- | Lens focusing on the 'idt_info' field of 'Identifier'.
--
-- @since 1.0.0
idtInfo :: Lens' Identifier SyntaxInfo
idtInfo = lens idt_info \s x -> s { idt_info = x }

-- | Compound lens focusing on @('idtInfo' . 'stxInfoScopes')@ field of an
-- 'Identifier'.
--
-- @since 1.0.0
idtScopes :: Lens' Identifier ScopeInfo
idtScopes = idtInfo . stxInfoScopes

-- Syntax ----------------------------------------------------------------------

-- | The 'Syntax' record represents syntax objects. A syntax object is a datum
-- equipped with lexical information.
--
-- @since 1.0.0
data Syntax
  = SyntaxVal  Value    {-# UNPACK #-} !SyntaxInfo
  | SyntaxList [Syntax] {-# UNPACK #-} !SyntaxInfo
  deriving (Eq, Generic, Lift, Ord)

-- | Pattern synonym for @('SyntaxVal' (ValueB _) _)@.
--
-- @since 1.0.0
pattern SyntaxB :: Bool -> SyntaxInfo -> Syntax
pattern SyntaxB x info = SyntaxVal (ValueB x) info

-- | Pattern synonym for @('SyntaxVal' (ValueC _) _)@.
--
-- @since 1.0.0
pattern SyntaxC :: Char -> SyntaxInfo -> Syntax
pattern SyntaxC x info = SyntaxVal (ValueC x) info

-- | Pattern synonym for @('SyntaxVal' (ValueS _) _)@.
--
-- @since 1.0.0
pattern SyntaxS :: Symbol -> SyntaxInfo -> Syntax
pattern SyntaxS x info = SyntaxVal (ValueS x) info

-- | Pattern synonym for @('SyntaxVal' (ValueF32 _) _)@.
--
-- @since 1.0.0
pattern SyntaxF32 :: Float -> SyntaxInfo -> Syntax
pattern SyntaxF32 x info = SyntaxVal (ValueF32 x) info

-- | Pattern synonym for @('SyntaxVal' (ValueI32 _) _)@.
--
-- @since 1.0.0
pattern SyntaxI32 :: Int32 -> SyntaxInfo -> Syntax
pattern SyntaxI32 x info = SyntaxVal (ValueI32 x) info

-- | Pattern synonym for @('SyntaxVal' (ValueLam _) _)@.
--
-- @since 1.0.0
pattern SyntaxLam :: Lambda -> SyntaxInfo -> Syntax
pattern SyntaxLam x info = SyntaxVal (ValueLam x) info

{-# COMPLETE
  SyntaxB, SyntaxC, SyntaxS, SyntaxF32, SyntaxI32, SyntaxLam, SyntaxList
  #-}

-- | @since 1.0.0
instance Display Syntax where
  display stx = case stx ^. syntaxDatum of
    DatumS    x -> Doc.char '#' <> display x
    DatumList x -> Doc.group (Doc.char '#' <> displayList x)
    datum       -> display datum

-- | @since 1.0.0
instance IsList Syntax where
  type Item Syntax = Syntax

  fromList stxs = SyntaxList stxs def

  toList (SyntaxList stxs _) = stxs
  toList _                   = errorWithoutStackTrace "(toList @Syntax): syntax object is not a list"

-- | @since 1.0.0
instance NFData Syntax

-- | @since 1.0.0
instance Show Syntax where
  show = Doc.pretty 80 . display

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
datumToSyntax info (DatumVal    val)  = SyntaxVal val info
datumToSyntax info (DatumList vals) = SyntaxList (map (datumToSyntax info) vals) info
datumToSyntax _    (DatumStx  stx)  = stx

-- | Converts a 'Syntax' object to a datum by stripping the syntax object's
-- lexical information.
--
-- @since 1.0.0
syntaxToDatum :: Syntax -> Datum
syntaxToDatum (SyntaxVal  val  _) = DatumVal val
syntaxToDatum (SyntaxList stxs _) = DatumList (map syntaxToDatum stxs)

-- Syntax - Scope Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntaxScope :: Maybe Phase -> Scope -> Syntax -> Syntax
syntaxScope ph sc (SyntaxVal val info) =
  let info' = over stxInfoScopes (ScopeInfo.insert ph sc) info
   in SyntaxVal val info'
syntaxScope ph sc (SyntaxList stxs info) =
  let stxs' = map (syntaxScope ph sc) stxs
      info' = over stxInfoScopes (ScopeInfo.insert ph sc) info
   in SyntaxList stxs' info'

-- | TODO: docs
--
-- @since 1.0.0
syntaxPrune :: Phase -> ScopeSet -> Syntax -> Syntax
syntaxPrune ph scps (SyntaxVal val info) =
  let info' = over stxInfoScopes (ScopeInfo.deletes ph scps) info
   in SyntaxVal val info'
syntaxPrune ph scps (SyntaxList stxs info) =
  let stxs' = map (syntaxPrune ph scps) stxs
      info' = over stxInfoScopes (ScopeInfo.deletes ph scps) info
   in SyntaxList stxs' info'

-- Syntax - Optics -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntaxDatum :: Lens' Syntax Datum
syntaxDatum = lens syntaxToDatum \s -> datumToSyntax (s ^. syntaxInfo)

-- | Lens focusing on the 'stx_info' field of 'Syntax'.
--
-- @since 1.0.0
syntaxInfo :: Lens' Syntax SyntaxInfo
syntaxInfo = lens getter setter
  where
    getter :: Syntax -> SyntaxInfo
    getter (SyntaxVal  _ info) = info
    getter (SyntaxList _ info) = info

    setter :: Syntax -> SyntaxInfo -> Syntax
    setter (SyntaxVal  val  _) info = SyntaxVal  val  info
    setter (SyntaxList stxs _) info = SyntaxList stxs info

-- | Compound lens focusing on @('stxInfo' . 'stxInfoProperties')@ field of a
-- 'Syntax'.
--
-- @since 1.0.0
syntaxProperties :: Lens' Syntax (HashMap Symbol Syntax)
syntaxProperties = syntaxInfo . stxInfoProperties

-- | Compound lens focusing on @('stxInfo' . 'stxInfoScopes')@ field of a
-- 'Syntax'.
--
-- @since 1.0.0
syntaxScopes :: Lens' Syntax ScopeInfo
syntaxScopes = syntaxInfo . stxInfoScopes

-- | Compound prism focusing on the @('datumBool' . 'syntaxDatum')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
syntaxBool :: Prism' Syntax Bool
syntaxBool = prism' (datumToSyntax def . DatumB) (preview datumBool . view syntaxDatum)

-- | Compound prism focusing on the @('datumChar' . 'syntaxDatum')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
syntaxChar :: Prism' Syntax Char
syntaxChar = prism' (datumToSyntax def . DatumC) (preview datumChar . view syntaxDatum)

-- | Compound prism focusing on the @('datumF32' . 'syntaxDatum')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
syntaxF32 :: Prism' Syntax Float
syntaxF32 = prism' (datumToSyntax def . DatumF32) (preview datumF32 . view syntaxDatum)

-- | Compound prism focusing on the @('datumI32' . 'syntaxDatum')@ constructor
-- of 'Datum'.
--
-- @since 1.0.0
syntaxI32 :: Prism' Syntax Int32
syntaxI32 = prism' (datumToSyntax def . DatumI32) (preview datumI32 . view syntaxDatum)

-- | TODO: docs
--
-- @since 1.0.0
syntaxId :: Prism' Syntax Identifier
syntaxId = prism' identifierToSyntax \case
  SyntaxS s info -> Just (Identifier s info)
  _              -> Nothing

-- | TODO: docs
--
-- @since 1.0.0
syntaxList :: Prism' Syntax [Syntax]
syntaxList = prism' construct deconstruct
  where
    construct :: [Syntax] -> Syntax
    construct []           = SyntaxList [] def
    construct (stx : stxs) =
      let info :: SyntaxInfo
          info = def { stx_info_source = stx ^. syntaxInfo ^. stxInfoSource }
       in SyntaxList (stx : stxs) info

    deconstruct :: Syntax -> Maybe [Syntax]
    deconstruct (SyntaxList stxs _) = Just stxs
    deconstruct _                   = Nothing

-- SyntaxInfo ------------------------------------------------------------------

-- | 'SyntaxInfo' is a record containing lexical information that is associated
-- with syntax objects such as 'Syntax' or 'Identifier'.
--
-- @since 1.0.0
data SyntaxInfo = SyntaxInfo
  { stx_info_source :: Maybe SourceInfo
    -- ^ TODO: docs
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
defaultSyntaxInfo = SyntaxInfo def def HashMap.empty

-- SyntaxInfo - Optics ---------------------------------------------------------

-- | Lens focusing on the 'stx_info_properties' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoSource :: Lens' SyntaxInfo (Maybe SourceInfo)
stxInfoSource = lens stx_info_source \s x -> s { stx_info_source = x }

-- | Lens focusing on the 'stx_info_srcloc' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoScopes :: Lens' SyntaxInfo ScopeInfo
stxInfoScopes = lens stx_info_scopes \s x -> s { stx_info_scopes = x }

-- | Lens focusing on the 'stx_info_properties' field of 'SyntaxInfo'.
--
-- @since 1.0.0
stxInfoProperties :: Lens' SyntaxInfo (HashMap Symbol Syntax)
stxInfoProperties = lens stx_info_properties \s x -> s { stx_info_properties = x }