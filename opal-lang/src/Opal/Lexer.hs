{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opal.Lexer
  ( -- * Lexer
    Lexer (L#, unL#),
    runLexer,
    runStringLexer,

    -- ** TODO
    Error (Error, errLoc, errLen, errSort, errSrc),
    newError,

    -- ** TODO
    ErrorSort (ExnEmptyApp, ExnChrMatch, ExnStrMatch),

    -- * Syntax
    lexStx,

    -- * Literals
    lexLit,
    lexBoolLit,

    -- * Primitive
    lexList,
    parentheses,
  )
where

import Data.Char (isSpace, isControl)
import Data.Functor (($>))
import Data.IO.FileBuffer (FileBuffer)
import Data.IO.FileBuffer qualified as FileBuffer

import GHC.Exts qualified as GHC

import Prelude hiding (exp, lex)

--------------------------------------------------------------------------------

import Opal.AST.Literal (Literal (BoolLit))

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Expand.Syntax (StxCtx (StxCtx), Syntax)
import Opal.Expand.Syntax qualified as Syntax

import Control.Monad.Except (catchError, throwError)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Lexer.Core qualified as Lexer
import Opal.Lexer.Error (Error (..), ErrorSort (..), newError)
import Opal.Lexer.Monad (Lexer (L#), unL#)
import Opal.Lexer.Monad qualified as Monad
import Data.SrcLoc (SrcLoc)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Opal.Lexer.Monad as Lexer

--------------------------------------------------------------------------------

runLexer :: FileBuffer -> Lexer a -> Either Error a
runLexer buf lexer = Monad.run buf lexer

runStringLexer :: String -> Lexer a -> Either Error a
runStringLexer str = runLexer (FileBuffer.fromString str)

-- Syntax ----------------------------------------------------------------------

lexStx :: Lexer Syntax
lexStx = do
  Lexer.whitespace
  catchError (alts lexStxAtoms) \exn -> do 
    (loc, stxs) <- catchError lexStxApp \_ -> throwError exn
    case NonEmpty.nonEmpty stxs of  
      Nothing -> Lexer.raiseLoc ExnEmptyApp loc
      Just {} -> pure (Syntax.App (StxCtx loc MultiScopeSet.empty) stxs)
  where 
    lexStxAtoms :: NonEmpty (String, Lexer Syntax)
    lexStxAtoms = 
      [ ("#'stx", lexQteStx)
      , ("literal", lexLitStx)
      , ("symbol", lexAtomStx)
      , ("(#%app . stx ...)", lexAtomStx)
      ]

lexQteStx :: Lexer Syntax
lexQteStx = do
  loc <- Monad.location
  stx <- Lexer.string "#'" *> lexStx
  let syntax :: Syntax
      syntax = Syntax.Idt $ Syntax.StxIdt (StxCtx loc MultiScopeSet.empty) (Symbol.pack "syntax")
   in pure (Syntax.App (StxCtx loc MultiScopeSet.empty) [syntax, stx])

lexLitStx :: Lexer Syntax
lexLitStx = do
  loc <- Monad.location
  Syntax.Lit (StxCtx loc MultiScopeSet.empty) <$> lexLit

lexAtomStx :: Lexer Syntax
lexAtomStx = do
  loc <- Monad.location
  fmap (Syntax.Idt . Syntax.StxIdt (StxCtx loc MultiScopeSet.empty)) lexSymbol

lexStxApp :: Lexer (SrcLoc, [Syntax])
lexStxApp = do
  loc <- Monad.location
  sxs <- lexList lexStx
  pure (loc, sxs)

-- Literal ---------------------------------------------------------------------

lexLit :: Lexer Literal
lexLit = alts [("boolean literal", lexBoolLit)]

lexBoolLit :: Lexer Literal
lexBoolLit = do
  loc <- Monad.location
  Lexer.single '#'
  altExn lexT lexF >>= \case 
    Left {} -> do 
      let issue = "expected #t or #f"
      Monad.raiseLoc (ExnReport issue) loc
    Right val -> do 
      Lexer.whitespace
      pure (BoolLit val)
  where
    lexT :: Lexer Bool
    lexT = Lexer.single 't' $> True

    lexF :: Lexer Bool
    lexF = Lexer.single 'f' $> False

-- Atoms -----------------------------------------------------------------------

lexSymbol :: Lexer Symbol
lexSymbol = Lexer.scan1 (not . isNameChar) Monad.finalize
  where 
    isNameChar :: Char -> Bool 
    isNameChar chr =  isSpace chr || isControl chr || elem chr "()[]{}\'\"\NUL"

lexList :: Lexer a -> Lexer [a]
lexList lexer = parentheses (multiple (lexer <* Lexer.whitespace))

--------------------------------------------------------------------------------

multiple :: forall a. Lexer a -> Lexer [a]
multiple (L# lex#) = run# id
  where
    run# :: ([a] -> [a]) -> Lexer [a]
    run# k =
      L# \buf# p# loc0# -> case lex# buf# p# loc0# of
        (# loc1#, (# _ | #) #) ->
          (# loc1#, (# | k [] #) #)
        (# loc1#, (# | x #) #) ->
          unL# (run# (GHC.oneShot \xs -> xs `seq` k (x : xs))) buf# p# loc1#

parentheses :: Lexer a -> Lexer a
parentheses lexer =
  flatAlt 
    (Lexer.between '(' ')' (Lexer.whitespace *> lexer <* Lexer.whitespace))
    (Lexer.between '[' ']' (Lexer.whitespace *> lexer <* Lexer.whitespace))

alts :: NonEmpty (String, Lexer a) -> Lexer a
alts ((nm0, lex) :| lxs) = do
  loc <- Monad.location
  catchError (foldr flatAlt lex $ map snd lxs) \_ -> do
    let issue = List.intercalate ", " (nm0 : map fst lxs)
    Monad.raiseLoc (ExnReport issue) loc

flatAlt :: Lexer a -> Lexer a -> Lexer a
flatAlt lex0 lex1 = altExn lex0 lex1 >>= either (throwError . fst) pure

altExn :: Lexer a -> Lexer a -> Lexer (Either (Error, Error) a)
altExn (L# f) (L# g) =
  L# \buf0# span0# loc0# ->
    case f buf0# span0# loc0# of
      (# loc1#, (# exn1 | #) #) -> case g buf0# span0# loc1# of
        (# loc2#, (# exn2 | #) #) -> (# loc2#, (# | Left (exn1, exn2) #) #)
        (# loc2#, (# | x #) #) -> (# loc2#, (# | Right x #) #)
      (# loc1#, (# | x #) #) -> (# loc1#, (# | Right x #) #)