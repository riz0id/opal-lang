{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Parse.Monad
  ( -- * TODO
    ParseError (ParseError, exn'syntax, exn'sort),

    -- * TODO
    ParseErrorSort
      ( ExnMissingProc,
        ExnLiteralProc,
        ExnBadCallArity,
        ExnBadSyntax
      ),

    -- * TODO
    ParseCtx (ParseCtx, ctx'focus, ctx'phase, ctx'binds),
    set'focus,

    -- * TODO
    ParseM (P, unP),
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)

import Data.Data (Data)
import Data.Kind (Type)
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

import Opal.Common.GenSym (MonadGenSym, newGenSym, newGenSymWith)
import Opal.Common.Symbol (Symbol)

import Control.Arrow (Arrow, ArrowApply, app, arr, first)
import Control.Category (Category, id, (.))
import Opal.Expand.Resolve.Class
  ( MonadResolve,
    newBind,
    newScopeId,
    resolve,
    resolveBind,
  )
import Opal.Expand.Resolve.Monad (ResolveM)
import Opal.Expand.Syntax (Syntax)
import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expand.Transformer (Transform)
import Prelude hiding ((.))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseError = ParseError
  { exn'syntax :: Syntax
  , exn'sort :: ParseErrorSort
  }
  deriving (Data, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseErrorSort :: Type where
  -- | Emitted when an application form without a function call, i.e. an
  -- unquoted expression of the form @()@.
  ExnMissingProc :: ParseErrorSort
  -- | Emitted when an application form call on a non-procedure literal like
  -- boolean, number, or etc...
  ExnLiteralProc :: ParseErrorSort
  ExnBadCallArity ::
    {-# UNPACK #-} !Int ->
    {-# UNPACK #-} !Int ->
    ParseErrorSort
  ExnBadSyntax :: ParseErrorSort
  deriving (Data, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseCtx e = ParseCtx
  { ctx'focus :: e
  , ctx'phase :: {-# UNPACK #-} !Phase
  , ctx'binds :: Map Symbol Transform
  }
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
instance Functor ParseCtx where
  fmap f (ParseCtx focus phase binds) = ParseCtx (f focus) phase binds
  {-# INLINE fmap #-}

-- | TODO
--
-- @since 1.0.0
set'focus :: e' -> ParseCtx e -> ParseCtx e'
set'focus stx (ParseCtx _ ph env) = ParseCtx stx ph env
{-# INLINE set'focus #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype ParseM (e :: Type) (a :: Type) :: Type where
  P :: {unP :: e -> ResolveM (Either ParseError a)} -> ParseM e a

-- | @since 1.0.0
instance Functor (ParseM e) where
  fmap f (P k) = P (fmap (fmap f) . k)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (ParseM e) where
  pure x = P \_ -> pure (Right x)
  {-# INLINE pure #-}

  P f <*> P g = P \ctx -> liftA2 (<*>) (f ctx) (g ctx)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad (ParseM e) where
  P k >>= f =
    P \ctx ->
      k ctx >>= \case
        Left exn -> pure (Left exn)
        Right rx -> unP (f rx) ctx
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ParseError (ParseM e) where
  throwError exn = P \_ -> pure (Left exn)
  {-# INLINE throwError #-}

  catchError (P k) f =
    P \ctx ->
      k ctx >>= \case
        Left exn -> unP (f exn) ctx
        Right rx -> pure (Right rx)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader e (ParseM e) where
  ask = P \ctx -> pure (Right ctx)
  {-# INLINE ask #-}

  local f (P k) = P (k . f)
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadGenSym (ParseM e) where
  newGenSym = P \_ -> fmap Right newGenSym
  {-# INLINE newGenSym #-}

  newGenSymWith idt = P \_ -> fmap Right (newGenSymWith idt)
  {-# INLINE newGenSymWith #-}

-- | @since 1.0.0
instance MonadResolve (ParseM e) where
  newScopeId = P \_ -> fmap Right newScopeId
  {-# INLINE newScopeId #-}

  newBind phase idt = P \_ -> fmap Right (newBind phase idt)
  {-# INLINE newBind #-}

  resolveBind phase idt = P \_ -> fmap Right (resolveBind phase idt)
  {-# INLINE resolveBind #-}

  resolve phase idt = P \_ -> fmap Right (resolve phase idt)
  {-# INLINE resolve #-}

-- | @since 1.0.0
instance Category ParseM where
  id = P (pure . Right)
  {-# INLINE id #-}

  P g . P f =
    P \ctx ->
      f ctx >>= \case
        Left exn -> pure (Left exn)
        Right rx -> g rx 
  {-# INLINE (.) #-}

-- | @since 1.0.0
instance Arrow ParseM where
  arr f = P (pure . Right . f)
  {-# INLINE arr #-}

  first (P k) =
    P \ctx ->
      k (fst ctx) >>= \case
        Left exn -> pure (Left exn)
        Right rx -> pure (Right (rx, snd ctx))
  {-# INLINE first #-}

-- | @since 1.0.0
instance ArrowApply ParseM where
  app = P \(P k, x) -> k x
  {-# INLINE app #-}