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
  , qexpToSyntaxP
    -- * QuasiList
  , QuasiList (..)
    -- ** Template Haskell
  , qlistToSyntaxE
  , qlistToSyntaxesE
  , qlistToSyntaxP
  , qlistToSyntaxesP
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
  , qvarToSyntaxP
  , qvarToSyntaxesP
    -- * QuasiVal
  , QuasiVal (..)
    -- ** Template Haskell
  , qvalToSyntaxE
  , qvalToSyntaxesE
  , qvalToSyntaxP
  , qvalToSyntaxesP
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

import Control.Lens (Lens', lens, preview, review, (^.))

import Data.Default (Default (..))
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.String (IsString(..))
import Data.Traversable (for)

import Language.Haskell.TH (Q, Exp (..), Name, Pat (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))

import Opal.Common.Symbol (Symbol, symbolToString)
import Opal.Common.TH (Pattern(..))
import Opal.Syntax
import Opal.Writer.Doc qualified as Doc (string)
import Opal.Writer.Class (Display(..))

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
qexpToSyntaxesE (QVal val)  = qvalToSyntaxesE val
qexpToSyntaxesE (QVar var)  = qvarToSyntaxesE var
qexpToSyntaxesE (QExp list) = qlistToSyntaxesE list

-- | TODO: docs
--
-- @since 1.0.0
qexpToSyntaxP :: QExp -> Q Pat
qexpToSyntaxP (QVal val)  = qvalToSyntaxP val
qexpToSyntaxP (QVar var)  = qvarToSyntaxP var
qexpToSyntaxP (QExp list) = qlistToSyntaxP list

-- | TODO: docs
--
-- @since 1.0.0
qexpToSyntaxesP :: QExp -> Q Pat
qexpToSyntaxesP (QVal val)  = qvalToSyntaxesP val
qexpToSyntaxesP (QVar var)  = qvarToSyntaxesP var
qexpToSyntaxesP (QExp list) = qlistToSyntaxesP list

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
  if null (getQuasiList list)
    then pure (VarE 'review `AppE` VarE 'syntaxList `AppE` ListE [])
    else do
      stxsE <- qlistToSyntaxesE list
      pure (VarE 'review `AppE` VarE 'syntaxList `AppE` stxsE)

-- | TODO: docs
--
-- @since 1.0.0
qlistToSyntaxesE :: QuasiList -> Q Exp
qlistToSyntaxesE (QuasiList list)
  | null list = pure (ListE [])
  | otherwise = do
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

-- | TODO: docs
--
-- @since 1.0.0
qlistToSyntaxP :: QuasiList -> Q Pat
qlistToSyntaxP list = do
  listP <- qlistToSyntaxesP list
  pure (ConP 'SyntaxList [] [listP, WildP])

-- | TODO: docs
--
-- @since 1.0.0
qlistToSyntaxesP :: QuasiList -> Q Pat
qlistToSyntaxesP (QuasiList list) = case unsnoc list of
  Nothing -> pure (ListP [])
  Just (inits, final) -> do
    initsP <- traverse qexpToSyntaxP inits
    finalP <- qexpToSyntaxesP final

    let listConsP :: Pat -> Pat -> Pat
        listConsP p1 p2 = InfixP p1 '(:) p2

    pure (foldr listConsP finalP initsP)
  where
    unsnoc :: Array a -> Maybe (Array a, a)
    unsnoc xs = case length xs of
      0   -> Nothing
      len ->
        let final = Array.indexArray xs (len - 1)
            inits = Array.cloneArray xs 0 (len - 1)
         in Just (inits, final)

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
    let kind = case k of { QuasiClassStx -> ""; _ -> show k }
        cls  = case e of { EllipsisNone -> ""; _ -> show e }
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
  EllipsisNone -> do
    let convertE = toSyntaxConverterE (var ^. qvarKind)
    let varE     = TH.varE (quasiVarToName var)
    [e| $convertE $varE |]
  _            -> do
    [e| Syntax $(qvarToDatumE var) def |]

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
qvarToSyntaxP var = case var ^. qvarEllipsis of
  EllipsisNone -> case var ^. qvarKind of
    QuasiClassId  -> do
      let varP  = VarP (quasiVarToName var)
      let viewE = VarE 'preview `AppE` VarE 'syntaxId
      pure (ViewP viewE (ConP 'Just [] [varP]))
    QuasiClassStx ->
      pure (VarP (quasiVarToName var))
    QuasiClassSymbol -> do
      let varP  = VarP (quasiVarToName var)
      let viewE = VarE 'preview `AppE` VarE 'syntaxSymbol
      pure (ViewP viewE (ConP 'Just [] [varP]))
    _ ->
      undefined
  EllipsisMany -> do
    varP <- qvarToSyntaxesP var
    pure (ConP 'SyntaxList [] [varP, WildP])
  EllipsisSome -> do
    varP <- qvarToSyntaxesP var
    pure (ConP 'SyntaxList [] [varP, WildP])

-- | TODO: docs
--
-- @since 1.0.0
qvarToSyntaxesP :: QuasiVar -> Q Pat
qvarToSyntaxesP var = do
  let varP = VarP (quasiVarToName var)
  case var ^. qvarEllipsis of
    EllipsisNone -> case var ^. qvarKind of
      QuasiClassBool   -> pure (ListP [ConP 'SyntaxB [] [varP, WildP]])
      QuasiClassChar   -> pure (ListP [ConP 'SyntaxC [] [varP, WildP]])
      QuasiClassF32    -> pure (ListP [ConP 'SyntaxF32 [] [varP, WildP]])
      QuasiClassI32    -> pure (ListP [ConP 'SyntaxI32 [] [varP, WildP]])
      QuasiClassLam    -> pure (ListP [ConP 'SyntaxLam [] [varP, WildP]])
      QuasiClassId     -> pure (ListP [ConP 'SyntaxId [] [varP]])
      QuasiClassStx    -> pure (ListP [varP])
      QuasiClassSymbol -> pure (ListP [ConP 'SyntaxS [] [varP, WildP]])
    EllipsisMany -> case var ^. qvarKind of
      QuasiClassStx -> pure varP
      cls           -> pure (ViewP (VarE 'traverse `AppE` toSyntaxViewerE cls) (ConP 'Just [] [varP]))
    EllipsisSome -> case var ^. qvarKind of
      QuasiClassStx -> do
        let viewE = VarE 'NonEmpty.nonEmpty
        pure (ViewP viewE (ConP 'Just [] [varP]))
      cls           -> do
        name <- TH.newName "x"
        let viewE =
              LamE [VarP name]
                (UInfixE
                  (VarE 'NonEmpty.nonEmpty `AppE` VarE name)
                  (VarE '(>>=))
                  (VarE 'traverse `AppE` toSyntaxViewerE cls))
        pure (ViewP viewE (ConP 'Just [] [varP]))

-- QuasiVal --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data QuasiVal
  = QuasiValB !Bool
    -- ^ A literal boolean quasi-value.
  | QuasiValC {-# UNPACK #-} !Char
    -- ^ A literal character quasi-value.
  | QuasiValS {-# UNPACK #-} !Symbol
    -- ^ A literal symbol quasi-value.
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
  case val of
    QuasiValB x -> do
      valE <- lift x
      pure (ConE 'SyntaxB `AppE` valE `AppE` infoE)
    QuasiValC x -> do
      valE <- lift x
      pure (ConE 'SyntaxC `AppE` valE `AppE` infoE)
    QuasiValS x -> do
      valE <- lift x
      pure (ConE 'SyntaxS `AppE` valE `AppE` infoE)

-- | TODO: docs
--
-- @since 1.0.0
qvalToSyntaxesE :: QuasiVal -> Q Exp
qvalToSyntaxesE val = [e| [$(qvalToSyntaxE val)] |]

-- | TODO: docs
--
-- @since 1.0.0
qvalToSyntaxP :: QuasiVal -> Q Pat
qvalToSyntaxP (QuasiValB x) = [p| SyntaxB $(liftPat x) _ |]
qvalToSyntaxP (QuasiValC x) = [p| SyntaxC $(liftPat x) _ |]
qvalToSyntaxP (QuasiValS x) = [p| SyntaxS $(liftPat x) _ |]

-- | TODO: docs
--
-- @since 1.0.0
qvalToSyntaxesP :: QuasiVal -> Q Pat
qvalToSyntaxesP val = do
  valP <- qvalToSyntaxP val
  pure (ListP [valP])

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
  | QuasiClassLam
    -- ^ TODO: docs
  | QuasiClassStx
    -- ^ TODO: docs
  | QuasiClassSymbol
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
  show QuasiClassBool   = "bool"
  show QuasiClassChar   = "char"
  show QuasiClassF32    = "f32"
  show QuasiClassI32    = "i32"
  show QuasiClassId     = "id"
  show QuasiClassLam    = "lam"
  show QuasiClassStx    = "stx"
  show QuasiClassSymbol = "symbol"

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
toSyntaxConverterE QuasiClassBool   = [e| \x -> SyntaxB   x def      |]
toSyntaxConverterE QuasiClassChar   = [e| \x -> SyntaxC   x def      |]
toSyntaxConverterE QuasiClassF32    = [e| \x -> SyntaxF32 x def      |]
toSyntaxConverterE QuasiClassI32    = [e| \x -> SyntaxI32 x def      |]
toSyntaxConverterE QuasiClassId     = [e| \x -> identifierToSyntax x |]
toSyntaxConverterE QuasiClassLam    = [e| \x -> SyntaxLam x def      |]
toSyntaxConverterE QuasiClassStx    = [e| \x -> x                    |]
toSyntaxConverterE QuasiClassSymbol = [e| \x -> SyntaxS x def        |]

-- | TODO: docs
--
-- @since 1.0.0
toSyntaxViewerE :: QuasiClass -> Exp
toSyntaxViewerE QuasiClassBool   = VarE 'preview `AppE` VarE 'syntaxBool
toSyntaxViewerE QuasiClassChar   = VarE 'preview `AppE` VarE 'syntaxChar
toSyntaxViewerE QuasiClassF32    = VarE 'preview `AppE` VarE 'syntaxF32
toSyntaxViewerE QuasiClassI32    = VarE 'preview `AppE` VarE 'syntaxI32
toSyntaxViewerE QuasiClassId     = VarE 'preview `AppE` VarE 'syntaxId
toSyntaxViewerE QuasiClassLam    = VarE 'preview `AppE` VarE 'syntaxLambda
toSyntaxViewerE QuasiClassStx    = VarE 'id
toSyntaxViewerE QuasiClassSymbol = VarE 'preview `AppE` VarE 'syntaxSymbol

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
toSyntaxesTransformerE EllipsisNone = [e| \f xs -> [f xs]                          |]
toSyntaxesTransformerE EllipsisMany = [e| \f xs -> map f xs                        |]
toSyntaxesTransformerE EllipsisSome = [e| \f xs -> foldr @NonEmpty ((:) . f) [] xs |]