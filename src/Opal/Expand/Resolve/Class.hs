
module Opal.Expand.Resolve.Class 
  ( MonadResolve,
    resolveName,
    resolveBind,
  )
where 

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Expand.Syntax (StxIdt)
import Opal.Expand.Syntax.Binding (Binding)
import Opal.Expand.Syntax.Binding qualified as Binding

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
resolveName :: MonadResolve m => StxIdt -> m Name
resolveName idt = fmap Binding.binder (resolveBind idt)

-- | TODO 
--
-- @since 1.0.0
class Monad m => MonadResolve m where 
  -- | TODO 
  --
  -- @since 1.0.0
  resolveBind :: StxIdt -> m Binding