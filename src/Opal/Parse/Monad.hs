{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Parse.Monad 
  ( -- * Parse
    Parse (..),

    -- * Re-exports
    module Opal.Parse.ParseContext,
    module Opal.Parse.ParseError,
  )
where 

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)

--------------------------------------------------------------------------------

import Opal.Parse.ParseError
import Opal.Parse.ParseContext

--------------------------------------------------------------------------------


-- | TODO
--
-- @since 1.0.0
newtype Parse a = Parse
  {unP :: ParseContext -> (# ParseError| a #)}

-- | @since 1.0.0
instance Functor Parse where
  fmap f (Parse k) =
    Parse \s -> case k s of
      (# e | #) -> (# e | #)
      (# | x #) -> (# | f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Parse where
  pure x = Parse \_ -> (# | x #)
  {-# INLINE pure #-}

  Parse f <*> Parse g =
    Parse \s -> case f s of
      (# e | #) -> (# e | #)
      (# | k #) -> case g s of
        (# e | #) -> (# e | #)
        (# | x #) -> (# | k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Parse where
  Parse k >>= f =
    Parse \s -> case k s of
      (# e | #) -> (# e | #)
      (# | x #) -> unP (f x) s
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ParseError Parse where
  throwError e = Parse \_ -> (# e | #)
  {-# INLINE throwError #-}

  catchError (Parse k) f =
    Parse \s -> case k s of
      (# e | #) -> unP (f e) s
      (# | x #) -> (# | x #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader ParseContext Parse where
  ask = Parse \s -> (# | s #)
  {-# INLINE ask #-}

  local f (Parse k) = Parse \s -> k (f s)
  {-# INLINE local #-}