{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Syntax
  ( -- * Syntax
    Syntax (StxAtom, StxList, content, context),

    -- ** Construction

    -- ** Scopes
    scope,
    flips,
    prune,

    -- * StxCtx
    StxCtx (StxCtx, location, length, multiscope),
  )
where

import Data.Data (Data)
import Data.SrcLoc (SrcLoc, posn, line, coln)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.MultiScopeSet (MultiScopeSet, Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

-- Syntax ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Syntax = Syntax
  { content :: Either Symbol [Syntax]
  , context :: {-# UNPACK #-} !StxCtx
  }
  deriving (Data, Eq, Ord)

-- @since 1.0.0
instance Show Syntax where 
  -- show (StxPrim ctx prim) = "(StxPrim " ++ shows ctx " " ++ shows prim ")"
  show (StxAtom ctx atom) = "(StxAtom " ++ shows ctx " " ++ shows atom ")"
  show (StxList ctx stxs) = "(StxList " ++ shows ctx " " ++ shows stxs ")"
  {-# INLINE show #-}

-- Syntax - Construction -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pattern StxAtom :: StxCtx -> Symbol -> Syntax
pattern StxAtom ctx atom = Syntax (Left atom) ctx

-- | TODO
--
-- @since 1.0.0
pattern StxList :: StxCtx -> [Syntax] -> Syntax
pattern StxList ctx stxs = Syntax (Right stxs) ctx

{-# COMPLETE StxAtom, StxList #-}

-- Syntax - Scopes -------------------------------------------------------------

-- | Extend the syntax object's scopes by the given scope at a particular phase.
--
-- @since 1.0.0
scope :: Phase -> ScopeId -> Syntax -> Syntax
scope ph sc (StxAtom ctx atom) = 
  let ctx' :: StxCtx 
      ctx' = adjust ph (ScopeSet.insert sc) ctx 
   in StxAtom ctx' atom
scope ph sc (StxList ctx stxs) = 
  let ctx' :: StxCtx 
      ctx' = adjust ph (ScopeSet.insert sc) ctx 
   in StxList ctx' (map (scope ph sc) stxs)

-- | Flip the syntax object's scopes by the given scope at a particular phase.
--
-- @since 1.0.0
flips :: Phase -> ScopeId -> Syntax -> Syntax
flips ph sc stx = stx {context = adjust ph (ScopeSet.flips sc) stx.context}

-- | Prunes the set of scopes from the syntax object's scopes at a particular 
-- phase.
--
-- @since 1.0.0
prune :: Phase -> ScopeSet -> Syntax -> Syntax
prune ph sc stx = stx {context = adjust ph (`ScopeSet.difference` sc) stx.context}

-- StxCtx ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StxCtx = StxCtx
  { location :: {-# UNPACK #-} !SrcLoc
  , length :: {-# UNPACK #-} !Int
  , multiscope :: MultiScopeSet
  }
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Show StxCtx where 
  show (StxCtx loc len sc) = 
    "(StxCtx:" 
      ++ shows loc.posn ":"
      ++ shows loc.line ":"
      ++ shows loc.coln " "
      ++ shows len " "
      ++ shows sc ")"
  {-# INLINE show #-}

-- | TODO
--
-- @since 1.0.0
adjust :: Phase -> (ScopeSet -> ScopeSet) -> StxCtx -> StxCtx
adjust ph f ctx = ctx {multiscope = MultiScopeSet.adjust ph f ctx.multiscope}