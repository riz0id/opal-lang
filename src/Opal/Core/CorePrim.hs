module Opal.Core.CorePrim
  ( -- * Core Primitives
    CorePrim
      ( CorePrimSyntaxLocalValue
      ),
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | TODO
--
-- @since
data CorePrim
  = CorePrimSyntaxLocalValue
  deriving (Data, Enum, Eq, Ord, Show)