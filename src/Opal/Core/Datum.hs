{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Core.Datum (
  -- * Datums
  Datum (..),

  -- ** Construction
  syntaxToDatum,
  datumToSyntax,
  syntaxExpr,

  -- ** Predicates
  isListDatum,
  isSymbolDatum,
  isPairDatum,
  isSyntaxDatum,
  isProcDatum,
) where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)

import Opal.Core.Prim (CorePrim)
import Opal.Core.SExp (SExp)

import Opal.Expand.Syntax (Syntax)
import Opal.Expand.Syntax qualified as Syntax
import Data.SrcLoc (SrcLoc)
import Opal.Expand.Syntax.StxCtx
import qualified Opal.Core.Prim as Core.Prim

-- Datums ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Datum
  = Stx {-# UNPACK #-} !Syntax
  | Atom {-# UNPACK #-} !Symbol
  | Bool Bool
  | Pair Datum Datum 
  | Prim CorePrim
  | Proc [Name] (NonEmpty (SExp Datum))
  | List [Datum]
  | Void 
  deriving (Data, Eq, Ord, Lift, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
syntaxToDatum :: Syntax -> Datum
syntaxToDatum (Syntax.StxVoid _) = Void
syntaxToDatum (Syntax.StxBool _ bool) = Bool bool
syntaxToDatum (Syntax.StxPair _ s s') = Pair (syntaxToDatum s) (syntaxToDatum s')
syntaxToDatum (Syntax.StxAtom _ name) = Atom name
syntaxToDatum (Syntax.StxList _ stxs) = List (map syntaxToDatum stxs)

datumToSyntax :: Syntax -> Datum -> Maybe SrcLoc -> Syntax
datumToSyntax orig datum loc = case datum of
  Stx stx -> stx
  Atom atom ->
    let stxctx :: StxCtx
        stxctx = orig.context {Syntax.location = loc}
     in Syntax.StxAtom stxctx atom
  Bool bool ->
    let stxctx :: StxCtx
        stxctx = orig.context {Syntax.location = loc}
     in Syntax.StxBool stxctx bool
  Pair a b ->
    let stxctx :: StxCtx
        stxctx = orig.context {Syntax.location = loc}
     in Syntax.StxPair stxctx (datumToSyntax orig a loc) (datumToSyntax orig b loc) 
  Prim prim ->
    let stxctx :: StxCtx
        stxctx = orig.context {Syntax.location = loc}
     in Syntax.StxAtom stxctx (Core.Prim.toSymbol prim)
  List dtms ->
    let stxctx :: StxCtx
        stxctx = orig.context {Syntax.location = loc}
     in Syntax.StxList stxctx (map (\dtm -> datumToSyntax orig dtm Nothing) dtms)


-- | TODO
--
-- @since 1.0.0
syntaxExpr :: Syntax -> Datum
syntaxExpr (Syntax.StxVoid _) = Void
syntaxExpr (Syntax.StxBool _ bool) = Bool bool
syntaxExpr (Syntax.StxPair _ s s') = Pair (Stx s) (Stx s')
syntaxExpr (Syntax.StxAtom _ atom) = Atom atom
syntaxExpr (Syntax.StxList _ stxs) = List (map Stx stxs)

-- Predicates ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
isListDatum :: Datum -> Bool
isListDatum List {} = True
isListDatum _ = False

-- | TODO
--
-- @since 1.0.0
isSymbolDatum :: Datum -> Bool
isSymbolDatum Atom {} = True
isSymbolDatum _ = False

-- | TODO
--
-- @since 1.0.0
isPairDatum :: Datum -> Bool
isPairDatum Pair {} = True
isPairDatum _ = False

-- | TODO
--
-- @since 1.0.0
isSyntaxDatum :: Datum -> Bool
isSyntaxDatum Stx {} = True
isSyntaxDatum _ = False

-- | TODO
--
-- @since 1.0.0
isProcDatum :: Datum -> Bool
isProcDatum Proc {} = True
isProcDatum _ = False