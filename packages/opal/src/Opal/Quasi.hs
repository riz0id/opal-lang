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
  , liftQExpAsSyntaxE
  , liftQExpAsSyntaxListE
    -- * QuasiVar
  , QuasiVar (..)
    -- ** Basic Operations
  , symbolToQuasiVar
  , quasiVarToName
    -- ** Lenses
  , quasiVarSymbol
  , quasiVarKind
  , quasiVarEllipsis
    -- ** Quotation
  , liftQuasiVarAsDatumListE
  , liftQuasiVarAsSyntaxListE
    -- * QuasiVal
  , QuasiVal (..)
    -- * EllipsisClass
  , EllipsisClass (..)
  )
where

import Control.Lens (Lens', lens, (^.))

import Data.Default (Default (..))
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Primitive.Array (Array)
import Data.String (IsString(..))
import Data.Traversable (for)

import Language.Haskell.TH (Q, Exp (..), Name)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))

import Opal.Common.Symbol (Symbol, symbolToString)
import Opal.Syntax (Datum (..), DatumKind (..), Syntax (..), SyntaxInfo, identifierToSyntax)
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
  | QExp {-# UNPACK #-} !(Array QExp)
    -- ^ A 'QExp' is a quasi-expression.
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Show QExp where
  show (QVal qval) = show qval
  show (QVar qvar) = show qvar
  show (QExp qexp) = show (toList qexp)

  showList xs = showChar '(' . showWords xs . showChar ')'

-- QExp - Readers --------------------------------------------------------------

-- QExp - Quotation ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
liftQExpAsSyntaxE :: QExp -> Q Exp
liftQExpAsSyntaxE (QVal val) = liftQuasiValAsSyntaxE val
liftQExpAsSyntaxE (QVar var) = do
  undefined
liftQExpAsSyntaxE (QExp exs) = do
  infoE <- lift (def :: SyntaxInfo)
  let stxsE :: Q Exp
      stxsE = liftQuasiExpAsSyntaxListE exs
  valE  <- [e| DatumList (map DatumStx $stxsE) |]
  pure (ConE 'Syntax `AppE` valE `AppE` infoE)

-- | TODO: docs
--
-- @since 1.0.0
liftQExpAsSyntaxListE :: QExp -> Q Exp
liftQExpAsSyntaxListE (QVal val) = liftQuasiValAsSyntaxListE val
liftQExpAsSyntaxListE (QVar var) = liftQuasiVarAsSyntaxListE var
liftQExpAsSyntaxListE (QExp exs) = liftQuasiExpAsSyntaxListE exs

-- | TODO: docs
--
-- @since 1.0.0
liftQuasiExpAsSyntaxListE :: Array QExp -> Q Exp
liftQuasiExpAsSyntaxListE exs = do
  defInfoE <- lift (def :: SyntaxInfo)

  listsE <- for exs \ex -> case ex of
    QVal val  -> liftQuasiValAsSyntaxListE val
    QVar var  -> liftQuasiVarAsSyntaxListE var
    QExp exs' -> do
      let stxE :: Q Exp
          stxE = liftQuasiExpAsSyntaxListE exs'
      valE <- [e| DatumList (map DatumStx $stxE) |]
      pure (ListE [ConE 'Syntax `AppE` valE `AppE` defInfoE])

  let listConcatE :: Exp -> Exp -> Exp
      listConcatE e1 e2 = UInfixE e1 (VarE '(++)) e2

  let listMappendE :: Array Exp -> Exp
      listMappendE = foldr listConcatE (ListE [])

  pure (listMappendE listsE)

-- QuasiVar --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data QuasiVar = QuasiVar
  { quasi_var_symbol   :: {-# UNPACK #-} !Symbol
    -- ^ The symbol representing the quasi-variable.
  , quasi_var_kind     :: Maybe DatumKind
    -- ^ If present, 'quasi_var_kind' is the kind of datum annotation that was
    -- appended to the quasi-variable,
    -- if one was given.
  , quasi_var_ellipsis :: Maybe EllipsisClass
    -- ^ If present, 'quasi_var_ellipsis' is the kind of ellipsis that followed
    -- the quasi-variable.
  }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display QuasiVar where
  display = fromString . show

-- | @since 1.0.0
instance Show QuasiVar where
  show (QuasiVar s k e) =
    let var  = '?' : symbolToString s
        kind = maybe "" ((:) ':' . show) k
        cls  = maybe "" ((:) ' ' . show) e
     in var ++ kind ++ cls

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
quasiVarToName var = TH.mkName (symbolToString (var ^. quasiVarSymbol))

-- QuasiVar - Lenses -----------------------------------------------------------

-- | Lens focusing on the 'quasi_var_symbol' field of a 'QuasiVar'.
--
-- @since 1.0.0
quasiVarSymbol :: Lens' QuasiVar Symbol
quasiVarSymbol = lens quasi_var_symbol \s x -> s { quasi_var_symbol = x }

-- | Lens focusing on the 'quasi_var_kind' field of a 'QuasiVar'.
--
-- @since 1.0.0
quasiVarKind :: Lens' QuasiVar (Maybe DatumKind)
quasiVarKind = lens quasi_var_kind \s x -> s { quasi_var_kind = x }

-- | Lens focusing on the 'quasi_list_ellipsis' field of a 'QuasiVar'.
--
-- @since 1.0.0
quasiVarEllipsis :: Lens' QuasiVar (Maybe EllipsisClass)
quasiVarEllipsis = lens quasi_var_ellipsis \s x -> s { quasi_var_ellipsis = x }

-- QuasiVar - Quotation --------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
liftQuasiVarAsDatumListE :: QuasiVar -> Q Exp
liftQuasiVarAsDatumListE var =
  let listE :: Q Exp
      listE = liftQuasiVarAsSyntaxListE var
   in [e| map DatumStx $listE |]

-- | TODO: docs
--
-- @since 1.0.0
liftQuasiVarAsSyntaxListE :: QuasiVar -> Q Exp
liftQuasiVarAsSyntaxListE var = case var ^. quasiVarKind of
  Nothing          -> pure varSyntaxListE
  Just DatumKindId ->
    let varE :: Q Exp
        varE = pure varSyntaxListE
     in [e| map identifierToSyntax $varE |]
  Just k           -> fail ("cannot bind " ++ show var ++ " with kind " ++ show k ++ ": unimplemented")
  where
    varSyntaxListE :: Exp
    varSyntaxListE =
      let varE :: Exp
          varE = VarE (quasiVarToName var)
       in case var ^. quasiVarEllipsis of
            Nothing           -> ListE [varE]
            Just EllipsisMany -> varE
            Just EllipsisSome -> VarE 'NonEmpty.toList `AppE` varE

-- QuasiVal --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data QuasiVal
  = QuasiValSymbol {-# UNPACK #-} !Symbol
    -- ^ TODO: docs
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Show QuasiVal where
  show (QuasiValSymbol s) = show s

-- QuasiVal - Quotation --------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
liftQuasiValAsSyntaxE :: QuasiVal -> Q Exp
liftQuasiValAsSyntaxE (QuasiValSymbol s) = do
  infoE   <- lift (def :: SyntaxInfo)
  symbolE <- lift s
  pure (ConE 'Syntax `AppE` (ConE 'DatumS `AppE` symbolE) `AppE` infoE)

-- | TODO: docs
--
-- @since 1.0.0
liftQuasiValAsSyntaxListE :: QuasiVal -> Q Exp
liftQuasiValAsSyntaxListE (QuasiValSymbol s) = do
  stxE <- liftQuasiValAsSyntaxE (QuasiValSymbol s)
  pure (ListE [stxE])

-- EllipsisClass ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data EllipsisClass
  = EllipsisMany
    -- ^ The 'EllipsisMany' class indicates a list of zero or more elements.
  | EllipsisSome
    -- ^ The 'EllipsisSome' class indicates a list of one or more elements.
  deriving (Enum, Eq, Ord)

-- | @since 1.0.0
instance Display EllipsisClass where
  display = fromString . show

-- | @since 1.0.0
instance Show EllipsisClass where
  show EllipsisMany = "..."
  show EllipsisSome = "...+"