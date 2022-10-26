{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Syntax (
  -- * Syntax
  Syntax (StxBool, StxAtom, StxList, content, context),

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

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

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
  deriving (Data, Eq, Ord)

-- @since 1.0.0
instance Show Syntax where
  show (StxBool ctx bool) = "(StxBool " ++ shows ctx " " ++ shows bool ")"
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
pattern StxAtom :: StxCtx -> Symbol -> Syntax
pattern StxAtom ctx atom = Syntax (Atom atom) ctx

-- | TODO
--
-- @since 1.0.0
pattern StxList :: StxCtx -> [Syntax] -> Syntax
pattern StxList ctx stxs = Syntax (List stxs) ctx

{-# COMPLETE StxBool, StxAtom, StxList #-}

-- | TODO
--
-- @since 1.0.0
makeSyntax :: Syntax -> Symbol -> Syntax
makeSyntax stx sym = StxAtom stx.context {length = Symbol.length sym} sym

-- Syntax - Scopes -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
scope :: Phase -> ScopeId -> Syntax -> Syntax
scope ph sc (StxBool ctx bool) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.insert sc) ctx
   in StxBool ctx' bool
scope ph sc (StxAtom ctx atom) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.insert sc) ctx
   in StxAtom ctx' atom
scope ph sc (StxList ctx stxs) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.insert sc) ctx
   in StxList ctx' (map (scope ph sc) stxs)

-- | TODO
--
-- @since 1.0.0
flips :: Phase -> ScopeId -> Syntax -> Syntax
flips ph sc (StxBool ctx bool) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.flips sc) ctx
   in StxBool ctx' bool
flips ph sc (StxAtom ctx atom) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.flips sc) ctx
   in StxAtom ctx' atom
flips ph sc (StxList ctx stxs) =
  let ctx' :: StxCtx
      ctx' = adjust ph (ScopeSet.flips sc) ctx
   in StxList ctx' (map (flips ph sc) stxs)

-- | TODO
--
-- @since 1.0.0
prune :: Phase -> ScopeSet -> Syntax -> Syntax
prune ph sc (StxBool ctx bool) =
  let ctx' :: StxCtx
      ctx' = adjust ph (`ScopeSet.difference` sc) ctx
   in StxBool ctx' bool
prune ph sc (StxAtom ctx atom) =
  let ctx' :: StxCtx
      ctx' = adjust ph (`ScopeSet.difference` sc) ctx
   in StxAtom ctx' atom
prune ph sc (StxList ctx stxs) =
  let ctx' :: StxCtx
      ctx' = adjust ph (`ScopeSet.difference` sc) ctx
   in StxList ctx' (map (prune ph sc) stxs)

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
  | List [Syntax]
  deriving (Data, Eq, Ord)