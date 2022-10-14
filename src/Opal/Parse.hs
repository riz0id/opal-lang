{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Parse
  ( -- * TODO
    Parse,
    evalParseExpr,
    runParseExpr,
    runParse,

    -- ** TODO
    ParseError
      ( ExnMissingProc,
        ExnParseLambda,
        ExnParseLetSyntax,
        ExnParseLetSyntaxBind,
        ExnParseStxIdt,
        ExnParseCase,
        ExnParseClause
      ),

    -- * TODO
    pSyntax,
    pVariable,
    pStxIdt,
    pStxLambda,
    pStxLetSyntax,
    pStxLetSyntaxBinds,
    pStxFormals,
    pStxFormalIdts,
  )
where

import Control.Lens ((^?))

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)

import Data.Traversable (for)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core
  ( Datum (DatumCore, DatumProc, DatumStx),
    Expr,
    SExp (SExpApp, SExpVal, SExpVar),
    syntaxToDatum,
  )
import Opal.Core.CoreForm (CoreForm (CoreFormLambda, CoreFormQuote, CoreFormSyntax))

import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax (StxCtx, StxIdt (StxIdt), Syntax (StxAtom, StxList), stxatom)
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding (Binder (BindCore, BindName))
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalParseExpr :: Syntax -> Either ParseError Expr
evalParseExpr stx =
  runParse (Phase 0) BindStore.coreSyntax do
    pSyntax (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
runParseExpr :: Phase -> BindStore -> Syntax -> Either ParseError Expr
runParseExpr ph store stx = runParse ph store (pSyntax stx)
{-# INLINE runParseExpr #-}

-- | TODO
--
-- @since 1.0.0
runParse :: Phase -> BindStore -> Parse a -> Either ParseError a
runParse ph store parser =
  case unP parser (ParseCtx ph store) of
    (# exn | #) -> Left exn
    (# | exp #) -> Right exp
{-# INLINE runParse #-}

-- | TODO
--
-- @since 1.0.0
newtype Parse a = Parse
  {unP :: ParseCtx -> (# ParseError| a #)}

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
instance MonadReader ParseCtx Parse where
  ask = Parse \s -> (# | s #)
  {-# INLINE ask #-}

  local f (Parse k) = Parse \s -> k (f s)
  {-# INLINE local #-}

-- TODO ------------------------------------------------------------------------

data ParseError
  = -- | TODO
    ExnMissingProc {-# UNPACK #-} !Syntax
  | -- | TODO
    ExnParseLambda [Syntax]
  | -- | TODO
    ExnParseCase [Syntax]
  | -- | TODO
    ExnParseClause Syntax
  | -- | TODO
    ExnParseLetSyntax [Syntax]
  | -- | TODO
    ExnParseLetSyntaxBind Syntax
  | -- | TODO
    ExnParseStxIdt Syntax
  | -- | TODO
    ExnParseSyntax [Syntax]
  | -- | TODO
    ExnParseQuote [Syntax]
  deriving (Eq, Ord, Show)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseCtx = ParseCtx
  { phase :: {-# UNPACK #-} !Phase
  , bindstore :: BindStore
  }
  deriving (Eq, Ord, Show)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
resolve :: StxCtx -> Symbol -> Parse Binder
resolve ctx symbol = do
  ph <- asks phase
  store <- asks bindstore
  -- TODO: Document why errors are ignore for parsing
  let idt :: StxIdt
      idt = StxIdt ctx symbol
   in case Resolve.runResolveId ph idt store of
        Left {} -> pure (BindName $ Symbol.toName symbol)
        Right binding -> pure binding.binder

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pSyntax :: Syntax -> Parse Expr
pSyntax (StxAtom ctx atom) = pStxAtom ctx atom
pSyntax (StxList ctx stxs) = case stxs of
  [] -> throwError (ExnMissingProc (StxList ctx stxs))
  stx : stxs' -> pStxList stx stxs'

-- | TODO
--
-- @since 1.0.0
pVariable :: Syntax -> Parse Name
pVariable stx = do
  idt <- pStxIdt stx
  pure (Symbol.toName idt.symbol)

-- | TODO
--
-- @since 1.0.0
pStxIdt :: Syntax -> Parse StxIdt
pStxIdt stx =
  case stx ^? stxatom of
    Nothing -> throwError (ExnParseStxIdt stx)
    Just idt -> pure idt

-- | TODO
--
-- @since 1.0.0
pStxAtom :: StxCtx -> Symbol -> Parse Expr
pStxAtom ctx atom = do
  bind <- resolve ctx atom
  case bind of
    BindCore prim -> pure (SExpVal $ DatumCore prim)
    BindName name -> pure (SExpVar name)

-- | TODO
--
-- @since 1.0.0
pStxList :: Syntax -> [Syntax] -> Parse Expr
pStxList (StxAtom ctx atom) stxs = do
  bind <- resolve ctx atom
  case bind of
    BindCore CoreFormLambda -> do
      case stxs of
        [stx1, stx2] -> do
          args <- pStxFormals stx1
          func <- pSyntax stx2
          pure (SExpVal $ DatumProc args func)
        _ -> throwError (ExnParseLambda stxs)
    BindCore CoreFormSyntax -> pStxSyntax stxs
    BindCore CoreFormQuote -> pStxQuote stxs
    BindCore prim -> do
      let func = SExpVal (DatumCore prim)
      args <- traverse pSyntax stxs
      pure (SExpApp func args)
    BindName name -> do
      let func = SExpVar name
      args <- traverse pSyntax stxs
      pure (SExpApp func args)
pStxList (StxList ctx stx) stxs = do
  func <- pSyntax (StxList ctx stx)
  args <- traverse pSyntax stxs
  pure (SExpApp func args)

-- | TODO
--
-- @since 1.0.0
pStxLambda :: [Syntax] -> Parse ([StxIdt], Syntax)
pStxLambda [stx, body] = do
  vars <- pStxFormalIdts stx
  pure (vars, body)
pStxLambda stxs = throwError (ExnParseLambda stxs)

-- | TODO
--
-- @since 1.0.0
pStxFormals :: Syntax -> Parse [Name]
pStxFormals stx = do
  idts <- pStxFormalIdts stx
  pure (map (Symbol.toName . Syntax.symbol) idts)

-- | TODO
--
-- @since 1.0.0
pStxFormalIdts :: Syntax -> Parse [StxIdt]
pStxFormalIdts (StxAtom ctx atom) = pure [StxIdt ctx atom]
pStxFormalIdts (StxList ctx vars) = do
  for vars \case
    StxAtom ctx' atom -> pure (StxIdt ctx' atom)
    StxList {} -> throwError (ExnParseLambda [StxList ctx vars])

-- | TODO
--
-- @since 1.0.0
pStxLetSyntax :: [Syntax] -> Parse ([(StxIdt, Syntax)], Syntax)
pStxLetSyntax [stx, stx'] = do
  bindings <- pStxLetSyntaxBinds stx
  pure (bindings, stx')
pStxLetSyntax stxs =
  throwError (ExnParseLetSyntax stxs)

-- | TODO
--
-- @since 1.0.0
pStxLetSyntaxBind :: Syntax -> Parse (StxIdt, Syntax)
pStxLetSyntaxBind (StxList _ [stx, stx']) = do
  idt <- pStxIdt stx
  pure (idt, stx')
pStxLetSyntaxBind stx = do
  throwError (ExnParseLetSyntaxBind stx)

-- | TODO
--
-- @since 1.0.0
pStxLetSyntaxBinds :: Syntax -> Parse [(StxIdt, Syntax)]
pStxLetSyntaxBinds (StxList _ stxs) = traverse pStxLetSyntaxBind stxs
pStxLetSyntaxBinds stx = throwError (ExnParseLetSyntaxBind stx)

-- | TODO
--
-- @since 1.0.0
pStxSyntax :: [Syntax] -> Parse Expr
pStxSyntax stxs
  | length stxs /= 1 = throwError (ExnParseSyntax stxs)
  | otherwise = pure (SExpVal $ DatumStx $ head stxs)

-- | TODO
--
-- @since 1.0.0
pStxQuote :: [Syntax] -> Parse Expr
pStxQuote stxs
  | length stxs /= 1 = throwError (ExnParseQuote stxs)
  | otherwise = pure (SExpVal $ syntaxToDatum $ head stxs)