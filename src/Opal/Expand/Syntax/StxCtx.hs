
module Opal.Expand.Syntax.StxCtx 
  ( -- * StxCtx
    StxCtx (..),
    
    -- ** Construction
    empty,

    -- ** Lenses 
    stxCtxSrcLoc,
    stxCtxMultiscope,

    -- ** Modification
    adjust,
  )
where 

import Control.Lens (Lens', lens, over)

import Data.Data (Data)
import Data.SrcLoc (SrcLoc)

import Language.Haskell.TH.Syntax (Lift)

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
  { location :: Maybe SrcLoc
  , multiscope :: MultiScopeSet
  }
  deriving (Data, Eq, Ord, Lift, Show)

-- Construction ----------------------------------------------------------------

-- | Constructs an empty syntax context. The resulting context has no scopes 
-- or lexical information.
--
-- @since 1.0.0
empty :: StxCtx 
empty = StxCtx Nothing MultiScopeSet.empty

-- Lenses ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stxCtxSrcLoc :: Lens' StxCtx (Maybe SrcLoc)
stxCtxSrcLoc = lens location \stxctx loc -> stxctx {location = loc}

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