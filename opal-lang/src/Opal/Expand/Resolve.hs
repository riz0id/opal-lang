{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Resolve
  ( -- * TODO
    MonadResolve,
    newScopeId,
    newBind,
    resolveBind,
    resolve,

    -- * TODO
    ResolveM (R, unR),
    evalResolveM,
  )
where

--------------------------------------------------------------------------------

import Opal.Expand.Resolve.Class
  ( MonadResolve,
    newBind,
    newScopeId,
    resolveBind,
    resolve,
  )
import Opal.Expand.Resolve.Monad (ResolveM (R, unR))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalResolveM :: ResolveM a -> a
evalResolveM (R k) = 
  case k mempty 0# 0# of
    (# _, _, _, result #) -> result
{-# INLINE evalResolveM #-}
