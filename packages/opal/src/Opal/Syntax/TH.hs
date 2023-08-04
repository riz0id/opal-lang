{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.TH
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Syntax.TH
  ( syntax,
    syntaxToPat,
  )
where

import Control.Lens ((^.))

import Data.Default (Default(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)

import Language.Haskell.TH (Loc (..), Pat (..), Q, Type (..), Exp (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax qualified as TH

import Opal.Common.Symbol
  ( Symbol (..)
  , eqSymbol
  , splitSymbol
  , symbolHead
  , symbolTail
  , symbolToString
  )
import Opal.Common.TH (Pattern (..))
import Opal.Reader (runStringReader)
import Opal.Syntax

import Text.Megaparsec (errorBundlePretty)
import Opal.Quasi.Reader (runQuasiReader, readQExp)
import Opal.Quasi (qexpToSyntaxE)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntax :: QuasiQuoter
syntax =
  QuasiQuoter
    { quoteExp  = readSyntaxExpression
    , quotePat  = readSyntaxPattern
    , quoteType = undefined -- FIXME: unimplemented
    , quoteDec  = undefined -- FIXME: unimplemented
    }

-- | TODO: docs
--
-- @since 1.0.0
readSyntaxQ :: String -> Q Syntax
readSyntaxQ input = do
  loc <- TH.location
  let filepath :: FilePath
      filepath = loc_filename loc
   in case runStringReader filepath input of
        Left  exn -> fail (errorBundlePretty exn)
        Right stx -> pure stx

-- | TODO: docs
--
-- @since 1.0.0
readSyntaxExpression :: String -> Q Exp
readSyntaxExpression input = do
  file <- fmap loc_filename TH.location
  qexp <- TH.runIO (runQuasiReader file input readQExp)
  qexpToSyntaxE qexp

-- | TODO: docs
--
-- @since 1.0.0
readSyntaxPattern :: String -> Q Pat
readSyntaxPattern input = do
  stx <- readSyntaxQ input
  syntaxToPat stx

-- | TODO: docs
--
-- @since 1.0.0
syntaxToPat :: Syntax -> Q Pat
syntaxToPat stx = do
  case syntaxToDatum stx of
    DatumB    val  -> do
      valP <- liftPat val
      pure (makeSyntaxPat (ConP 'DatumB [] [valP]) WildP)
    DatumC    val  -> do
      valP <- liftPat val
      pure (makeSyntaxPat (ConP 'DatumC [] [valP]) WildP)
    DatumS    val  ->
      identifierToSyntaxPat (Identifier val (stx ^. stxInfo))
    DatumF32  val  -> do
      valP <- liftPat val
      pure (makeSyntaxPat (ConP 'DatumF32 [] [valP]) WildP)
    DatumI32  val  -> do
      valP <- liftPat val
      pure (makeSyntaxPat (ConP 'DatumI32 [] [valP]) WildP)
    DatumLam  val  ->
      fail ("lambda cannot be used as a pattern: " ++ show val)
    DatumList vals ->
      patSyntaxList (map (datumToSyntax (stx ^. stxInfo)) vals)
    DatumStx  stx' ->
      syntaxToPat stx'

-- | TODO: docs
--
-- @since 1.0.0
identifierToSyntaxPat :: Identifier -> Q Pat
identifierToSyntaxPat idt@(Identifier s _)
  | isWildcardSymbol s = pure WildP
  | isBindingSymbol s  = makePatVariable idt False
  | otherwise          = do
    symbolP <- liftPat s
    pure (ConP 'Syntax [] [ConP 'DatumS [] [symbolP], WildP])

-- | TODO: docs
--
-- @since 1.0.0
isBindingSymbol :: Symbol -> Bool
isBindingSymbol s = symbolHead s == '?'

-- | TODO: docs
--
-- @since 1.0.0
isWildcardSymbol :: Symbol -> Bool
isWildcardSymbol s = symbolHead s == '_'

-- | TODO: docs
--
-- @since 1.0.0
patSyntaxList :: [Syntax] -> Q Pat
patSyntaxList stxs = case NonEmpty.nonEmpty stxs of
  Nothing ->
    let datumEmptyList :: Pat
        datumEmptyList = ConP '[] [ConT ''Datum] []
     in pure (makeSyntaxPat (ConP 'DatumList [] [datumEmptyList]) WildP)
  Just stxs'
    | isManyList stxs' -> manySyntaxesToSyntaxPat (NonEmpty.init stxs')
    | isSomeList stxs' -> someSyntaxesToSyntaxPat (NonEmpty.init stxs')
    | otherwise -> do
      -- FIXME: unimplemented
      pats <- for stxs \stx -> do
        pat <- syntaxToPat stx
        pure (ConP 'DatumStx [] [pat])

      let patList :: Pat
          patList = foldr makeListConPat patEmptyList pats

      pure (makeSyntaxPat (ConP 'DatumList [] [patList]) WildP)
  where
    patEmptyList :: Pat
    patEmptyList = ConP '[] [ConT ''Datum] []

    makeListConPat :: Pat -> Pat -> Pat
    makeListConPat pat rest = ConP '(:) [ConT ''Datum] [pat, rest]

-- | TODO: docs
--
-- @since 1.0.0
isManyList :: NonEmpty Syntax -> Bool
isManyList stxs =
  case syntaxToDatum (NonEmpty.last stxs) of
    DatumS s -> s `eqSymbol` "..."
    _        -> False

-- | TODO: docs
--
-- @since 1.0.0
isSomeList :: NonEmpty Syntax -> Bool
isSomeList stxs =
  case syntaxToDatum (NonEmpty.last stxs) of
    DatumS s -> s `eqSymbol` "...+"
    _        -> False

-- | TODO: docs
--
-- @since 1.0.0
manySyntaxesToSyntaxPat :: [Syntax] -> Q Pat
manySyntaxesToSyntaxPat stxs = case NonEmpty.nonEmpty stxs of
  Nothing    -> do
    let stx = Syntax (DatumList (map DatumStx stxs)) def
    fail ("expected variable pattern before '...' pattern: " ++ show stx)
  Just stxs' -> do
    let stxLast = NonEmpty.last stxs'

    case syntaxToDatum stxLast of
      DatumS s
        | isWildcardSymbol s -> pure WildP
        | isBindingSymbol  s -> do
          lastP <- makePatVariable (Identifier s def) True
          viewE <- [e| map (datumToSyntax $(TH.lift (def :: SyntaxInfo))) |]

          stxInits <- for (NonEmpty.init stxs') \stx -> do
            pat <- syntaxToPat stx
            pure (ConP 'DatumStx [] [pat])

          let makeListConPat :: Pat -> Pat -> Pat
              makeListConPat pat rest = ConP '(:) [ConT ''Datum] [pat, rest]

          let stxsP :: Pat
              stxsP = foldr makeListConPat (ViewP viewE lastP) stxInits

          pure (makeSyntaxPat (ConP 'DatumList [] [stxsP]) WildP)
      _ -> do
        let stx = Syntax (DatumList (map DatumStx stxs)) def
        fail ("expected variable pattern before '...' pattern: " ++ show stx)

-- | TODO: docs
--
-- @since 1.0.0
someSyntaxesToSyntaxPat :: [Syntax] -> Q Pat
someSyntaxesToSyntaxPat stxs = case NonEmpty.nonEmpty stxs of
  Nothing    -> fail ("expected variable pattern before '...+' pattern: " ++ show stxs)
  Just stxs' -> do
    let stxLast  = NonEmpty.last stxs'
    let infoLast = stxLast ^. stxInfo

    patLast <- case syntaxToDatum stxLast of
      DatumS s
        | isWildcardSymbol s -> pure WildP
        | isBindingSymbol  s -> do
          patVar  <- makePatVariable (Identifier s infoLast) True
          expView <- [e| \x -> fmap (NonEmpty.map (datumToSyntax $(TH.lift infoLast))) (NonEmpty.nonEmpty x) |]
          pure (ViewP expView (ConP 'Just [] [patVar]))
      _ -> fail ("expected variable pattern before '...+' pattern: " ++ show stxs)

    stxInits <- for (NonEmpty.init stxs') \stx -> do
        pat <- syntaxToPat stx
        pure (ConP 'DatumStx [] [pat])

    let patStxs :: Pat
        patStxs = foldr makeListConPat patLast stxInits

    pure (makeSyntaxPat (ConP 'DatumList [] [patStxs]) WildP)
  where
    makeListConPat :: Pat -> Pat -> Pat
    makeListConPat pat rest = ConP '(:) [ConT ''Datum] [pat, rest]

-- | TODO: docs
--
-- @since 1.0.0
makePatVariable :: Identifier -> Bool -> Q Pat
makePatVariable idt isList =
  case symbolTail (idt ^. idtSymbol) of
    Nothing  -> fail ("the symbol " ++ show idt ++ " can not be used as a pattern variable")
    Just var -> case splitSymbol (':' ==) var of
      Just (s', k)
        | k `eqSymbol` ":id" -> do
          let name = TH.mkName (symbolToString s')
          if isList
            then do
              let varP = ConP 'Just [ConT ''[] `AppT` ConT ''Identifier] [VarP name]
              pure (ViewP (VarE 'traverse `AppE` VarE 'syntaxToIdentifier) varP)
            else do
              let varP = ConP 'Just [ConT ''Identifier] [VarP name]
              pure (ViewP (VarE 'syntaxToIdentifier) varP)
        | otherwise ->
          fail ("unrecognized annotation: " ++ show idt)
      _  ->
        pure (VarP (TH.mkName (symbolToString var)))

-- | Create a v'Syntax' constructor pattern given a 'Datum' and 'SyntaxInfo'
-- patterns.
--
-- @since 1.0.0
makeSyntaxPat :: Pat -> Pat -> Pat
makeSyntaxPat patDatum patInfo = ConP 'Syntax [] [patDatum, patInfo]
