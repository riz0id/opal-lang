{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Quasi
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
module Opal.Quasi
  ( -- * QExp
    QExp (..)
    -- ** Template Haskell
  , qexpToSyntaxE
  , qexpToSyntaxesE
    -- * QuasiList
  , QuasiList (..)
    -- ** Template Haskell
  , qlistToSyntaxE
  , qlistToSyntaxesE
    -- * QuasiVar
  , QuasiVar (..)
    -- ** Basic Operations
  , symbolToQuasiVar
  , quasiVarToName
    -- ** Lenses
  , qvarSymbol
  , qvarKind
  , qvarEllipsis
    -- ** Template Haskell
  , qvarToDatumE
  , qvarToSyntaxE
  , qvarToDatumsE
  , qvarToSyntaxesE
    -- * QuasiVal
  , QuasiVal (..)
    -- ** Template Haskell
  , qvalToSyntaxE
  , qvalToSyntaxesE
  , qvalToSyntaxP
    -- * EllipsisClass
  , QuasiClass (..)
    -- ** Template Haskell
  , toSyntaxConverterE
    -- * EllipsisClass
  , EllipsisClass (..)
    -- ** Template Haskell
  , toSyntaxesTransformerE
  )
where

import Control.Lens (Lens', lens, (^.))

import Data.Default (Default (..))
import Data.Foldable (Foldable(..))
import Data.Primitive.Array (Array)
import Data.String (IsString(..))
import Data.Traversable (for)

import Language.Haskell.TH (Body (..), Q, Exp (..), Match (..), Name, Pat (..), Type (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))

import Opal.Common.Symbol (Symbol, symbolToString)
import Opal.Syntax (Datum (..), Syntax (..), SyntaxInfo, identifierToSyntax)
import Opal.Writer qualified as Doc
import Opal.Writer.Class (Display(..))
import Opal.Common.TH (Pattern(..))

--------------------------------------------------------------------------------

showWords :: Show a => [a] -> ShowS
showWords []       = showString ""
showWords (x : xs) = shows x . run xs
  where
    run []       = id
    run (y : ys) = showChar ' ' . shows y . run ys

-- QExp ------------------------------------------------------------------------

-- | 'QExp' is the type of quasi-expressions. A quasi-expression is a
-- intermediate representation of Opal expressions that are reflected as Haskell
-- expressions or patterns in quasi-quoters.
--
-- @since 1.0.0
data QExp
  = QVal {-# UNPACK #-} !QuasiVal
    -- ^ A 'QVar' is a quasi-variable.
  | QVar {-# UNPACK #-} !QuasiVar
    -- ^ A 'QVar' is a quasi-variable.
  | QExp {-# UNPACK #-} !QuasiList
    -- ^ A 'QExp' is a quasi-expression.
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Show QExp where
  show (QVal val)  = show val
  show (QVar var)  = show var
  show (QExp list) = show list

  showList xs = showChar '(' . showWords xs . showChar ')'

-- QExp - Template Haskell -----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
qexpToSyntaxE :: QExp -> Q Exp
qexpToSyntaxE (QVal val)  = qvalToSyntaxE val
qexpToSyntaxE (QVar var)  = qvarToSyntaxE var
qexpToSyntaxE (QExp list) = qlistToSyntaxE list

-- | TODO: docs
--
-- @since 1.0.0
qexpToSyntaxesE :: QExp -> Q Exp
qexpToSyntaxesE (QVal val) = qvalToSyntaxesE val
qexpToSyntaxesE (QVar var) = qvarToSyntaxesE var
qexpToSyntaxesE (QExp exs) = qlistToSyntaxesE exs

-- QuasiList -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype QuasiList = QuasiList
  { getQuasiList :: Array QExp }
  deriving (Eq, Ord)

instance Show QuasiList where
  show (QuasiList qexp) = show (toList qexp)

  showList xs = showChar '(' . showWords xs . showChar ')'

-- QuasiList - Template Haskell ------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
qlistToSyntaxE :: QuasiList -> Q Exp
qlistToSyntaxE list = do
  infoE <- lift (def :: SyntaxInfo)
  stxsE <- qlistToSyntaxesE list

  let valsE :: Exp
      valsE = VarE 'map `AppE` ConE 'DatumStx `AppE` stxsE

  let listE :: Exp
      listE = ConE 'DatumList `AppE` valsE

  pure (ConE 'Syntax `AppE` listE `AppE` infoE)

-- | TODO: docs
--
-- @since 1.0.0
qlistToSyntaxesE :: QuasiList -> Q Exp
qlistToSyntaxesE (QuasiList list) = do
  listsE <- for list \ex -> case ex of
    QVal val   -> qvalToSyntaxesE val
    QVar var   -> qvarToSyntaxesE var
    QExp list' -> do
      stxE <- qlistToSyntaxE list'
      pure (ListE [stxE])

  let listConcatE :: Exp -> Exp -> Exp
      listConcatE e1 e2 = UInfixE e1 (VarE '(++)) e2

  let listMappendE :: Array Exp -> Exp
      listMappendE = foldr listConcatE (ListE [])

  pure (listMappendE listsE)

-- QuasiVar --------------------------------------------------------------------

-- | 'QuasiVar' is a Haskell variable reference from a quasi-quoted Opal
-- expression.
--
-- @since 1.0.0
data QuasiVar = QuasiVar
  { qvar_symbol   :: {-# UNPACK #-} !Symbol
    -- ^ The symbol representing the quasi-variable.
  , qvar_kind     :: QuasiClass
    -- ^ The is the kind of datum annotation that was appended to the
    -- quasi-variable, if one was given. If no annotation was given, then the
    -- 'qvar_kind' will be 'QuasiClassSyntax'.
  , qvar_ellipsis :: EllipsisClass
    -- ^ The kind of ellipsis that followed the quasi-variable, if one was
    -- given. If no ellipsis was given, then the 'qvar_ellipsis' will be
    -- 'EllipsisNone'.
  }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display QuasiVar where
  display = fromString . show

-- | @since 1.0.0
instance Show QuasiVar where
  show (QuasiVar s k e) =
    let kind = case k of
          QuasiClassStx -> ""
          _             -> show k

        cls  = case e of
          EllipsisNone -> ""
          _            -> show e
     in '?' : symbolToString s ++ kind ++ cls

-- QuasiVar - Basic Operations -------------------------------------------------

-- | Convert a 'Symbol' into a 'QuasiVar'.
--
-- @since 1.0.0
symbolToQuasiVar :: Symbol -> QuasiVar
symbolToQuasiVar s = QuasiVar s def def

-- | Convert a 'Symbol' into a 'Name'.
--
-- @since 1.0.0
quasiVarToName :: QuasiVar -> Name
quasiVarToName var = TH.mkName (symbolToString (var ^. qvarSymbol))

-- QuasiVar - Lenses -----------------------------------------------------------

-- | Lens focusing on the 'qvar_symbol' field of a 'QuasiVar'.
--
-- @since 1.0.0
qvarSymbol :: Lens' QuasiVar Symbol
qvarSymbol = lens qvar_symbol \s x -> s { qvar_symbol = x }

-- | Lens focusing on the 'qvar_kind' field of a 'QuasiVar'.
--
-- @since 1.0.0
qvarKind :: Lens' QuasiVar QuasiClass
qvarKind = lens qvar_kind \s x -> s { qvar_kind = x }

-- | Lens focusing on the 'qvar_ellipsis' field of a 'QuasiVar'.
--
-- @since 1.0.0
qvarEllipsis :: Lens' QuasiVar EllipsisClass
qvarEllipsis = lens qvar_ellipsis \s x -> s { qvar_ellipsis = x }

-- QuasiVar - Template Haskell -------------------------------------------------

-- | Convert a 'QuasiVar' into a datum represented as a Haskell expression.
--
-- @since 1.0.0
qvarToDatumE :: QuasiVar -> Q Exp
qvarToDatumE var = [e| DatumList $(qvarToDatumsE var) |]

-- | Convert a 'QuasiVar' into a syntax object represented as a Haskell
-- expression.
--
-- @since 1.0.0
qvarToSyntaxE :: QuasiVar -> Q Exp
qvarToSyntaxE var = case var ^. qvarEllipsis of
  EllipsisNone -> TH.varE (quasiVarToName var)
  _            -> [e| Syntax $(qvarToDatumE var) def |]

-- | Convert a 'QuasiVar' into a list of datums represented as a Haskell
-- expression.
--
-- @since 1.0.0
qvarToDatumsE :: QuasiVar -> Q Exp
qvarToDatumsE var = [e| $transformerE (DatumList . $converterE) $varE |]
  where
    varE :: Q Exp
    varE = TH.varE (quasiVarToName var)

    converterE :: Q Exp
    converterE = toSyntaxConverterE (var ^. qvarKind)

    transformerE :: Q Exp
    transformerE = toSyntaxesTransformerE (var ^. qvarEllipsis)

-- | Convert a 'QuasiVar' into a list of syntax objects represented as a
-- Haskell expression.
--
-- @since 1.0.0
qvarToSyntaxesE :: QuasiVar -> Q Exp
qvarToSyntaxesE var = [e| $transformerE $converterE $varE |]
  where
    varE :: Q Exp
    varE = TH.varE (quasiVarToName var)

    converterE :: Q Exp
    converterE = toSyntaxConverterE (var ^. qvarKind)

    transformerE :: Q Exp
    transformerE = toSyntaxesTransformerE (var ^. qvarEllipsis)

-- | TODO: docs
--
-- @since 1.0.0
qvarToSyntaxP :: QuasiVar -> Q Pat
qvarToSyntaxP var = undefined

-- QuasiVal --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data QuasiVal
  = QuasiValB !Bool
    -- ^ TODO: docs
  | QuasiValC {-# UNPACK #-} !Char
    -- ^ TODO: docs
  | QuasiValS {-# UNPACK #-} !Symbol
    -- ^ TODO: docs
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Show QuasiVal where
  show (QuasiValB x) = if x then "#t" else "#f"
  show (QuasiValC c) = ['#', '\\', c]
  show (QuasiValS s) = show s

-- QuasiVal - Template Haskell -------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
qvalToSyntaxE :: QuasiVal -> Q Exp
qvalToSyntaxE val = do
  infoE <- lift (def :: SyntaxInfo)

  valE <- case val of
    QuasiValB x -> do
      boolE <- lift x
      pure (ConE 'DatumB `AppE` boolE)
    QuasiValC c -> do
      charE <- lift c
      pure (ConE 'DatumC `AppE` charE)
    QuasiValS s -> do
      symbolE <- lift s
      pure (ConE 'DatumS `AppE` symbolE)

  pure (ConE 'Syntax `AppE` valE `AppE` infoE)

-- | TODO: docs
--
-- @since 1.0.0
qvalToSyntaxesE :: QuasiVal -> Q Exp
qvalToSyntaxesE val = do
  stxE <- qvalToSyntaxE val
  pure (ListE [stxE])

-- | TODO: docs
--
-- @since 1.0.0
qvalToSyntaxP :: QuasiVal -> Q Pat
qvalToSyntaxP val =
  let valP :: Q Pat
      valP = case val of
        QuasiValB x -> do
          viewE <- [e| preview datumBool |]
          boolP <- liftPat x
          pure (ViewP viewE (ConP 'Just [] [boolP]))
        QuasiValC c -> do
          viewE <- [e| preview datumChar |]
          charP <- liftPat c
          pure (ViewP viewE (ConP 'Just [] [charP]))
        QuasiValS s -> do
          viewE <- [e| preview datumSymbol |]
          symbolP <- liftPat s
          pure (ViewP viewE (ConP 'Just [] [symbolP]))

   in [p| Syntax $valP _ |]

-- QuasiClass ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data QuasiClass
  = QuasiClassBool
    -- ^ TODO: docs
  | QuasiClassChar
    -- ^ TODO: docs
  | QuasiClassF32
    -- ^ TODO: docs
  | QuasiClassI32
    -- ^ TODO: docs
  | QuasiClassId
    -- ^ TODO: docs
  | QuasiClassStx
    -- ^ TODO: docs
  deriving (Enum, Eq, Ord)

-- | @since 1.0.0
instance Default QuasiClass where
  def = QuasiClassStx

-- | @since 1.0.0
instance Display QuasiClass where
  display = Doc.string . show

-- | @since 1.0.0
instance Show QuasiClass where
  show QuasiClassBool = "bool"
  show QuasiClassChar = "char"
  show QuasiClassF32  = "f32"
  show QuasiClassI32  = "i32"
  show QuasiClassId   = "id"
  show QuasiClassStx  = "stx"

-- QuasiClass - Template Haskell -----------------------------------------------

-- | Lift an 'QuasiClass' to a Haskell expression. The resulting Haskell
-- expression will be a function in one argument mapping to a syntax object. The
-- type of the function is determined by the given 'QuasiClass':
--
--   * 'QuasiClassBool' will result in a function expression with type
--      @'Bool' -> 'Syntax'@.
--
--   * 'QuasiClassChar' will result in a function expression with type
--      @'Char' -> 'Syntax'@.
--
--   * 'QuasiClassF32' will result in a function expression with type
--      @'Float' -> 'Syntax'@.
--
--   * 'QuasiClassI32' will result in a function expression with type
--      @'In32' -> 'Syntax'@.
--
--   * 'QuasiClassId' will result in a function expression with type
--      @'Identifier' -> 'Syntax'@.
--
--   * 'QuasiClassStx' will result in a function expression with type
--      @'Syntax' -> 'Syntax'@.
--
-- @since 1.0.0
toSyntaxConverterE :: QuasiClass -> Q Exp
toSyntaxConverterE QuasiClassBool = [e| \x -> Syntax (DatumB   x) def |]
toSyntaxConverterE QuasiClassChar = [e| \x -> Syntax (DatumC   x) def |]
toSyntaxConverterE QuasiClassF32  = [e| \x -> Syntax (DatumF32 x) def |]
toSyntaxConverterE QuasiClassI32  = [e| \x -> Syntax (DatumI32 x) def |]
toSyntaxConverterE QuasiClassId   = [e| \x -> identifierToSyntax  x   |]
toSyntaxConverterE QuasiClassStx  = [e| \x -> x                       |]

-- EllipsisClass ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data EllipsisClass
  = EllipsisNone
    -- ^ The 'EllipsisNone' class indicates a single syntax object.
  | EllipsisMany
    -- ^ The 'EllipsisMany' class indicates a list of zero or more syntax
    -- objects.
  | EllipsisSome
    -- ^ The 'EllipsisSome' class indicates a list of one or more syntax
    -- objects.
  deriving (Enum, Eq, Ord)

-- | @since 1.0.0
instance Default EllipsisClass where
  def = EllipsisNone

-- | @since 1.0.0
instance Display EllipsisClass where
  display = fromString . show

-- | @since 1.0.0
instance Show EllipsisClass where
  show EllipsisNone = "none"
  show EllipsisMany = "..."
  show EllipsisSome = "...+"

-- EllipsisClass - Template Haskell --------------------------------------------

-- | Lift an 'EllipsisClass' to a Haskell expression. The resulting Haskell
-- expression will be a function in two arguments mapping to a list of syntax
-- objects, i.e a @['Syntax']@. The type of the function's arguments is
-- determined by the given 'EllipsisClass':
--
--   * 'EllipsisNone' will result in a function with type
--      @(a -> 'Syntax') -> a -> ['Syntax']@.
--
--   * 'EllipsisMany' will result in a function with type
--      @(a -> 'Syntax') -> [a] -> ['Syntax']@.
--
--   * 'EllipsisSome' will result in a function with type
--      @(a -> 'Syntax') -> NonEmpty a -> ['Syntax']@.
--
-- @since 1.0.0
toSyntaxesTransformerE :: EllipsisClass -> Q Exp
toSyntaxesTransformerE EllipsisNone = [e| \f xs -> [f xs]                |]
toSyntaxesTransformerE EllipsisMany = [e| \f xs -> map f xs              |]
toSyntaxesTransformerE EllipsisSome = [e| \f xs -> foldr ((:) . f) [] xs |]