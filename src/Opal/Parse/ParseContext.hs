
module Opal.Parse.ParseContext 
  ( -- * ParseContext
    ParseContext (..),
    makeParseContext,
  )
where 


--------------------------------------------------------------------------------

import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.MultiScopeSet (Phase)

-- ParseContext ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseContext = ParseContext
  { ctx'phase :: {-# UNPACK #-} !Phase
  , ctx'bindstore :: BindStore
  }
  deriving (Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
makeParseContext :: Phase -> ParseContext 
makeParseContext phase = 
  ParseContext 
    { ctx'phase = phase 
    , ctx'bindstore = BindStore.coreSyntax
    }
{-# INLINE CONLIKE makeParseContext #-}