{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Parse
  ( -- * TODO
    Parse,
    evalParseProgram,
    evalParseDecl,
    evalParseExpr,
    runParseExpr,
    runParse,

    -- ** TODO
    ParseError (..),

    -- * TODO
    pProgram,
    pSyntax,
    pVariable,
    pStxIdt,
    pSyntaxModule,
    pStxLambda,
    pStxLet,
    pStxLetSyntax,
    pStxLetSyntaxBind,
    pStxLetSyntaxBinds,
    pStxDefineValue,
    pStxFormals,
    pStxFormalIdts,
  )
where


import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)

import Data.Traversable (for)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core (Expr, Decl (..), SExp (..))
import Opal.Core.Datum qualified as Datum

import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax (StxCtx, StxIdt (StxIdt), Syntax (..))
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Opal.Core.Prim as Core.Prim

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalParseProgram :: Syntax -> Either ParseError [Decl]
evalParseProgram stx =
  runParse (Phase 0) do
    pProgram (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
evalParseDecl :: Syntax -> Either ParseError Decl
evalParseDecl stx =
  runParse (Phase 0) do
    pDeclaration (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
evalParseExpr :: Syntax -> Either ParseError Expr
evalParseExpr stx =
  runParse (Phase 0) do
    pSyntax (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
runParseExpr :: Phase -> Syntax -> Either ParseError Expr
runParseExpr ph stx = runParse ph (pSyntax stx)
{-# INLINE runParseExpr #-}

-- | TODO
--
-- @since 1.0.0
runParse :: Phase -> Parse a -> Either ParseError a
runParse ph parser =
  case unP parser (ParseCtx ph BindStore.coreSyntax) of
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
    ExnParseLet [Syntax]
  | -- | TODO 
    ExnParseIf Syntax [Syntax]
  | -- | TODO 
    ExnParseModule Syntax
  | -- | TODO
    ExnParseLetBind Syntax
  | -- | TODO
    ExnParseLetSyntax [Syntax]
  | -- | TODO
    ExnParseLetSyntaxBind Syntax
  | -- | TODO
    ExnParseSyntax [Syntax]
  | -- | TODO
    ExnParseQuasiSyntax [Syntax]
  | -- | TODO
    ExnParseStxIdt Syntax
  | -- | TODO 
    ExnParseDefineValue [Syntax]
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
resolve :: StxCtx -> Symbol -> Parse Name
resolve ctx symbol = do
  ph <- asks phase
  store <- asks bindstore
  -- TODO: Document why errors are ignore for parsing
  let idt :: StxIdt
      idt = StxIdt ctx symbol
   in case Resolve.runResolveId ph idt store of
        Left {} -> pure (Symbol.toName symbol)
        Right binding -> pure binding.binder

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pProgram :: Syntax -> Parse [Decl]
pProgram (StxList _ stxs) = traverse pDeclaration stxs 
pProgram stx = fmap pure (pDeclaration stx)


-- | TODO
--
-- @since 1.0.0
pSyntaxModule :: Syntax -> Parse (StxIdt, [Syntax])
pSyntaxModule (StxList _ (stx : stxs)) = do 
  moduleIdt <- pStxIdt stx
  pure (moduleIdt, stxs)
pSyntaxModule stx = do 
  throwError (ExnParseModule stx)

-- | TODO
--
-- @since 1.0.0
pDeclaration :: Syntax -> Parse Decl 
pDeclaration stx@(StxList _ [StxAtom ctx atom, stx1, stx2]) = do 
  name <- resolve ctx atom
  case name of 
    "define-value" -> do 
      defn <- pStxIdt stx1
      sexp <- pSyntax stx2
      pure (DeclDefn defn sexp)
    "define-syntax-value" -> do 
      defn <- pStxIdt stx1
      sexp <- pSyntax stx2
      pure (DeclDefnStx defn sexp)
    _ -> do 
      fmap DeclSExp (pSyntax stx)
pDeclaration stx = do 
  fmap DeclSExp (pSyntax stx)

-- | TODO
--
-- @since 1.0.0
pSyntax :: Syntax -> Parse Expr
pSyntax (StxBool _ bool) = 
  pure (SExpVal (Datum.Bool bool)) 
pSyntax (StxPair _ stx0 stx1) = do 
  lhs <- pSyntax stx0 
  rhs <- pSyntax stx1
  pure (SExpApp (SExpVal (Datum.Prim Core.Prim.Cons)) [lhs, rhs]) 
pSyntax (StxAtom ctx atom) = 
  pStxAtom ctx atom
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
pStxIdt (StxAtom ctx atom) = pure (StxIdt ctx atom)
pStxIdt stx = throwError (ExnParseStxIdt stx)

-- | TODO
--
-- @since 1.0.0
pStxAtom :: StxCtx -> Symbol -> Parse Expr
pStxAtom ctx atom = fmap SExpVar (resolve ctx atom)

-- | TODO
--
-- @since 1.0.0
pStxList :: Syntax -> [Syntax] -> Parse Expr
pStxList (StxBool ctx atom) stxs = 
  pure (error ("attempt to call boolean as a procedure"))
pStxList (StxPair ctx s s') stxs = 
  pure (error ("attempt to call pair as a procedure"))
pStxList (StxAtom ctx atom) stxs = do
  resolve ctx atom >>= \case
    "quote" -> do 
      pStxQuote stxs
    "quote-syntax" -> do 
      pStxSyntax stxs
    "quasisyntax" -> do 
      pStxSyntax stxs
    "if" -> do
      case stxs of 
        [stx'c, stx'e0, stx'e1] -> do
          expr'c <- pSyntax stx'c
          expr'e0 <- pSyntax stx'e0
          expr'e1 <- pSyntax stx'e1
          pure (SExpIf expr'c expr'e0 expr'e1)
        _ -> do 
          throwError (ExnParseIf (StxAtom ctx atom) stxs)
    "let" -> do
      (vars, body) <- pLet stxs
      pure (SExpLet vars body)
    "lambda" -> do
      case stxs of
        stx : stxs' -> 
          case NonEmpty.nonEmpty stxs' of 
            Nothing -> throwError (ExnParseLambda stxs)
            Just xs -> do 
              args <- pStxFormals stx
              body <- traverse pSyntax xs
              pure (SExpVal $ Datum.Proc args body)
        _ -> throwError (ExnParseLambda stxs)
    name -> do
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
pStxLambda :: [Syntax] -> Parse ([StxIdt], [Syntax])
pStxLambda (stx : body) = do
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
pStxFormalIdts (StxBool _ _) = 
  pure (error ("using boolean as formal"))
pStxFormalIdts (StxPair _ _ _) = 
  pure (error ("using pair as formal"))
pStxFormalIdts (StxAtom ctx atom) = 
  pure [StxIdt ctx atom]
pStxFormalIdts (StxList ctx vars) = do
  for vars \case
    StxBool _ _ -> pure (error ("using boolean as formal"))
    StxPair _ _ _ -> pure (error ("using pair as formal"))
    StxAtom ctx' atom -> pure (StxIdt ctx' atom)
    StxList {} -> throwError (ExnParseLambda [StxList ctx vars])

-- | TODO
--
-- @since 1.0.0
pLet :: [Syntax] -> Parse (Map Name Expr, Expr)
pLet [stx, stx'] = do 
  vars <- pLetBinds stx
  body <- pSyntax stx'
  pure (vars, body)
pLet stxs = 
  throwError (ExnParseLet stxs)

-- | TODO
--
-- @since 1.0.0
pLetBinds :: Syntax -> Parse (Map Name Expr)
pLetBinds (StxList _ stxs) = do 
  vars <- traverse pLetBind stxs
  pure (Map.fromList vars)
pLetBinds stx = 
  throwError (ExnParseLetBind stx)

-- | TODO
--
-- @since 1.0.0
pLetBind :: Syntax -> Parse (Name, Expr)
pLetBind (StxList _ [stx, stx']) = do 
  name <- pVariable stx
  expr <- pSyntax stx'
  pure (name, expr)
pLetBind stx = 
  throwError (ExnParseLetBind stx)

-- | TODO
--
-- @since 1.0.0
pStxLet :: [Syntax] -> Parse (Map StxIdt Syntax, Syntax)
pStxLet [stx, stx'] = do
  bindings <- pStxLetSyntaxBinds stx
  pure (Map.fromList bindings, stx')
pStxLet stxs =
  throwError (ExnParseLetSyntax stxs)

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
pStxDefineValue :: [Syntax] -> Parse (StxIdt, Syntax)
pStxDefineValue [StxAtom ctx atom, stx] = pure (StxIdt ctx atom, stx) 
pStxDefineValue stxs = throwError (ExnParseDefineValue stxs)

-- | TODO
--
-- @since 1.0.0
pStxSyntax :: [Syntax] -> Parse Expr
pStxSyntax [stx] = pure (SExpVal $ Datum.Stx stx)
pStxSyntax stxs = throwError (ExnParseSyntax stxs)

-- | TODO
--
-- @since 1.0.0
pStxQuote :: [Syntax] -> Parse Expr
pStxQuote stxs
  | length stxs /= 1 = throwError (ExnParseQuote stxs)
  | otherwise = pure (SExpVal $ Datum.syntaxToDatum $ head stxs)