
module Opal.Expand.Syntax.StxCtx 
  ( -- * StxCtx
    StxCtx (..),
    
    -- ** Lenses 
    stxCtxSrcLoc,
    stxCtxLength,
    stxCtxMultiscope,

    -- ** Modification
    adjust,
  )
where 

import Control.Lens (Lens', lens, over)

import Data.Data (Data)
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc

import Prelude hiding (length)

--------------------------------------------------------------------------------

import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Syntax.MultiScopeSet (Phase, MultiScopeSet)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet

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

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
-- makeStxCtx :: Phase -> ScopeSet -> StxCtx -> StxCtx
-- makeStxCtx ph scopes ctx = ctx {multiscope = MultiScopeSet.singleton ph scopes}

-- Lenses ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stxCtxSrcLoc :: Lens' StxCtx SrcLoc
stxCtxSrcLoc = lens location \stxctx loc -> stxctx {location = loc}

-- | TODO
--
-- @since 1.0.0
stxCtxLength :: Lens' StxCtx Int
stxCtxLength = lens length \stxctx len -> stxctx {length = len}

-- | TODO
--
-- @since 1.0.0
stxCtxMultiscope :: Lens' StxCtx MultiScopeSet 
stxCtxMultiscope = lens multiscope \stxctx mxs -> stxctx {multiscope = mxs}

-- Modification ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
adjust :: Phase -> (ScopeSet -> ScopeSet) -> StxCtx -> StxCtx
adjust ph f = over stxCtxMultiscope (MultiScopeSet.adjust ph f) 