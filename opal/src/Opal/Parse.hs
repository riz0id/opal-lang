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
    pStxLambda,
    pStxLetSyntax,
    pStxLetSyntaxBinds,
    pStxFormals,
    pStxFormalIdts,
  )
where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)

import Data.List qualified as List
import Data.Traversable (for)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core
  ( Datum (DatumAtom, DatumList, DatumPrim, DatumProc, DatumStx),
    Expr,
    SExp (SExpApp, SExpVal, SExpVar),
    syntaxToDatum,
  )
import Opal.Core qualified as Core
import Opal.Core.Datum (Clause (Clause), Datum (DatumCase), Procedure (Procedure))
import Opal.Core.Prim (Prim (PrimCase, PrimClauseDef, PrimLambda, PrimQuote, PrimSyntax))

import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax (StxCtx, StxIdt (StxIdt), Syntax (StxAtom, StxList))
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding (Binder (BindName, BindPrim))
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
    ExnParseLetSyntax Syntax
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
pSyntax (StxList ctx stxs)
  | null stxs = throwError (ExnMissingProc (StxList ctx stxs))
  | otherwise = pStxList (head stxs) (tail stxs)

-- | TODO
--
-- @since 1.0.0
pStxIdt :: Syntax -> Parse StxIdt
pStxIdt (StxAtom ctx atom) = pure (StxIdt ctx atom)
pStxIdt stx = throwError (ExnParseStxIdt stx)

-- | TODO
--
-- @since 1.0.0
pStxAtom :: StxCtx -> Symbol -> Parse Expr
pStxAtom ctx atom = do
  bind <- resolve ctx atom
  case bind of
    BindPrim prim -> pure (SExpVal $ DatumPrim prim)
    BindName name -> pure (SExpVar name)

-- | TODO
--
-- @since 1.0.0
pStxList :: Syntax -> [Syntax] -> Parse Expr
pStxList (StxAtom ctx atom) stxs = do
  bind <- resolve ctx atom
  case bind of
    BindPrim PrimCase -> do
      match <- pStxCase stxs
      pure (SExpVal match)
    BindPrim PrimLambda -> do
      func <- pStxLambda stxs
      pure (SExpVal $ DatumProc func.formals func.body)
    BindPrim PrimSyntax -> pStxSyntax stxs
    BindPrim PrimQuote -> pStxQuote stxs
    BindPrim prim -> do
      let func = SExpVal (DatumPrim prim)
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
pStxLambda :: [Syntax] -> Parse Procedure
pStxLambda stxs
  | length stxs /= 2 = do
      let exn = ExnParseLambda stxs
      throwError exn
  | otherwise = do
      vars <- pStxFormals (stxs List.!! 0)
      body <- pSyntax (stxs List.!! 1)
      pure (Procedure vars body)

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
    StxList {} -> throwError (ExnParseLambda $ [StxList ctx vars])

-- | TODO
--
-- @since 1.0.0
pStxCase :: [Syntax] -> Parse Datum
pStxCase stxs
  | length stxs < 1 = throwError (ExnParseCase stxs)
  | otherwise = do
      scrut <- pSyntax (head stxs)
      cases <- traverse pStxClause (tail stxs)
      pure (DatumCase scrut cases)

-- | TODO
--
-- @since 1.0.0
pStxClause :: Syntax -> Parse Clause
pStxClause (StxList _ [StxAtom ctx atom, stx]) = do
  bind <- resolve ctx atom
  if bind == BindPrim PrimClauseDef
    then do
      let pat = DatumPrim PrimClauseDef
      body <- pSyntax stx
      pure (Clause pat body)
    else throwError (ExnParseClause $ StxAtom ctx atom)
pStxClause (StxList _ [StxList _ stx1, stx2]) = do
  pats <- traverse pStxPattern stx1
  body <- pSyntax stx2
  pure (Clause (DatumList pats) body)
pStxClause stx =
  throwError (ExnParseClause $ stx)

-- | TODO
--
-- @since 1.0.0
pStxPattern :: Syntax -> Parse Datum
pStxPattern (StxAtom ctx atom) = do
  bind <- resolve ctx atom
  case bind of
    BindPrim prim -> pure (DatumPrim prim)
    BindName name -> pure (DatumAtom $ Symbol.Symbol name)
pStxPattern (StxList _ stxs) = do
  dtms <- traverse pStxPattern stxs
  pure (DatumList dtms)

-- | TODO
--
-- @since 1.0.0
pStxLetSyntax :: Syntax -> Parse ([(StxIdt, Syntax)], Syntax)
pStxLetSyntax (StxList _ [stx, stx']) = do 
  bindings <- pStxLetSyntaxBinds stx
  pure (bindings, stx')
pStxLetSyntax stx = 
  throwError (ExnParseLetSyntax stx)

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
  | otherwise = pure (SExpVal $ DatumStx $ stxs List.!! 0)

-- | TODO
--
-- @since 1.0.0
pStxQuote :: [Syntax] -> Parse Expr
pStxQuote stxs
  | length stxs /= 1 = throwError (ExnParseQuote stxs)
  | otherwise = pure (SExpVal $ syntaxToDatum $ stxs List.!! 0)