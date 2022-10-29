{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Syntax (
  -- * Syntax
  Syntax (StxBool, StxPair, StxAtom, StxList, content, context),

  -- ** Construction
  makeSyntax,

  -- ** Scopes
  scope,
  flips,
  prune,
  index,

  -- * StxIdt
  StxIdt (..),
  scopeIdt,

  -- * StxCtx
  module Opal.Expand.Syntax.StxCtx,
) where

import Data.Data (Data)

import GHC.Records (HasField, getField)

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Syntax.StxCtx 

-- Syntax ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Syntax = Syntax
  { content :: StxData
  , context :: {-# UNPACK #-} !StxCtx
  }
  deriving (Data, Eq, Ord, Lift)

-- @since 1.0.0
instance Show Syntax where
  show (StxBool ctx bool) = "(StxBool " ++ shows ctx " " ++ shows bool ")"
  show (StxPair ctx s0 s1) = "(StxPair " ++ shows ctx " " ++ shows s0 " " ++ shows s1 ")"
  show (StxAtom ctx atom) = "(StxAtom " ++ shows ctx " " ++ shows atom ")"
  show (StxList ctx stxs) = "(StxList " ++ shows ctx " " ++ shows stxs ")"
  {-# INLINE show #-}

-- Syntax - Construction -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pattern StxBool :: StxCtx -> Bool -> Syntax
pattern StxBool ctx atom = Syntax (Bool atom) ctx

-- | TODO
--
-- @since 1.0.0
pattern StxPair :: StxCtx -> Syntax -> Syntax -> Syntax
pattern StxPair ctx stx1 stx2 = Syntax (Pair stx1 stx2) ctx

-- | TODO
--
-- @since 1.0.0
pattern StxAtom :: StxCtx -> Symbol -> Syntax
pattern StxAtom ctx atom = Syntax (Atom atom) ctx

-- | TODO
--
-- @since 1.0.0
pattern StxList :: StxCtx -> [Syntax] -> Syntax
pattern StxList ctx stxs = Syntax (List stxs) ctx

{-# COMPLETE StxBool, StxPair, StxAtom, StxList #-}

-- | TODO
--
-- @since 1.0.0
makeSyntax :: Syntax -> Symbol -> Syntax
makeSyntax stx = StxAtom stx.context 

-- Syntax - Scopes -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
scope :: Phase -> ScopeId -> Syntax -> Syntax
scope ph sc (StxList ctx stxs) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.insert sc) ctx
   in StxList ctx' (map (scope ph sc) stxs)
scope ph sc stx =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.insert sc) stx.context
   in Syntax stx.content ctx'

-- | TODO
--
-- @since 1.0.0
flips :: Phase -> ScopeId -> Syntax -> Syntax
flips ph sc (StxList ctx stxs) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.flips sc) ctx
   in StxList ctx' (map (flips ph sc) stxs)
flips ph sc stx =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.flips sc) stx.context
   in Syntax stx.content ctx' 

-- | TODO
--
-- @since 1.0.0
prune :: Phase -> ScopeSet -> Syntax -> Syntax
prune ph sc (StxList ctx stxs) =
  let ctx' :: StxCtx
      ctx' = adjust ph (`ScopeSet.difference` sc) ctx
   in StxList ctx' (map (prune ph sc) stxs)
prune ph sc stx =
  let ctx' :: StxCtx
      ctx' = adjust ph (`ScopeSet.difference` sc) stx.context
   in Syntax stx.content ctx' 

-- | TODO
--
-- @since 1.0.0
index :: Phase -> Syntax -> ScopeSet
index ph stx = MultiScopeSet.index ph stx.context.multiscope

-- StxIdt ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StxIdt = StxIdt
  { context :: {-# UNPACK #-} !StxCtx
  , symbol :: {-# UNPACK #-} !Symbol
  }
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance HasField "syntax" StxIdt Syntax where
  getField (StxIdt ctx atom) = StxAtom ctx atom
  {-# INLINE CONLIKE getField #-}

-- | TODO
--
-- @since 1.0.0
scopeIdt :: Phase -> ScopeId -> StxIdt -> StxIdt
scopeIdt ph sc (StxIdt ctx sym) = StxIdt (adjust ph (ScopeSet.insert sc) ctx) sym

-- StxData ---------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data StxData 
  = Bool Bool
  | Atom Symbol
  | Pair Syntax Syntax
  | List [Syntax]
  deriving (Data, Eq, Lift, Ord)