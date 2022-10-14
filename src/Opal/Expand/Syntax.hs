{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Syntax
  ( -- * Syntax
    Syntax (StxAtom, StxList, content, context),

    -- ** Construction

    -- ** Prisms
    stxatom,

    -- ** Scopes
    scope,
    flips,
    prune,
    index,

    -- * StxIdt
    StxIdt (StxIdt, context, symbol),
    scopeIdt,
    
    -- * StxCtx
    StxCtx (StxCtx, location, length, multiscope),

    -- ** Construction 
    makeStxCtx,
  )
where

import Control.Lens (Prism', prism')

import Data.Data (Data)
import Data.SrcLoc (SrcLoc, posn, line, coln)

import GHC.Records (HasField, getField)

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

-- Syntax - Prisms -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stxatom :: Prism' Syntax StxIdt
stxatom = prism' from to 
  where 
    from :: StxIdt -> Syntax 
    from (StxIdt ctx atom) = StxAtom ctx atom 

    to :: Syntax -> Maybe StxIdt 
    to (StxAtom ctx atom) = Just (StxIdt ctx atom) 
    to _ = Nothing
{-# INLINE stxatom #-}

-- Syntax - Scopes -------------------------------------------------------------

-- | TODO 
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

-- | TODO 
--
-- @since 1.0.0
flips :: Phase -> ScopeId -> Syntax -> Syntax
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
index ph (StxAtom ctx _) = MultiScopeSet.index ph (multiscope ctx)
index ph (StxList ctx _) = MultiScopeSet.index ph (multiscope ctx)

-- StxCtx ----------------------------------------------------------------------

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

-- StxCtx - Construction -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeStxCtx :: Phase -> ScopeSet -> StxCtx -> StxCtx 
makeStxCtx ph scopes ctx = ctx{multiscope = MultiScopeSet.singleton ph scopes} 

-- | TODO
--
-- @since 1.0.0
adjust :: Phase -> (ScopeSet -> ScopeSet) -> StxCtx -> StxCtx
adjust ph f ctx = ctx {multiscope = MultiScopeSet.adjust ph f ctx.multiscope}