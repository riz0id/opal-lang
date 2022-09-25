{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Parse
  ( -- * Parse
    Parse (P#, unP#),
    runStringParse,
    runBufferParse,

    -- * Errors
    Error (Error, exn'code, exn'name, exn'begin, exn'end, exn'posn, exn'span),
    ErrorSort
      ( ExnEmptyApp,
        ExnChrMatch,
        ExnStrMatch,
        ExnBalance,
        ExnFeedEoF,
        ExnReport
      ),
    makeError,
    raise,

    -- * Parse Scope
    label,
    scope,
    scope'begin,
    scope'end,

    -- * Parse State
    lookahead,
    peek,
    feed,
    take,
    advance,
    advanceN,

    -- * Source Information
    source'begin,
    source'end,
    source'span,

    -- * Combinators
    between,
    star,
    star1,
    parens,
    bracks,

    -- ** Alternatives
    ParseBranch (BranchFail, BranchLeft, BranchRight),
    branch,
    attempt,
    optional,
    alt,

    -- ** Patterns
    satisfy,
    single,
    string,
    whitespace,

    -- ** Scans
    scan,
    scan1,
    takeWhile,
    takeWhile_,
    takeWhile1,
  )
where

import Control.Applicative (liftA2)

import Control.Monad (replicateM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (get, gets, modify, put)

import Data.Char (isSpace)
import Data.CharArray.Prim qualified as CharArray
import Data.Functor (($>))
import Data.IO.FileBuffer (FileBuffer (FB))
import Data.IO.FileBuffer qualified as FileBuffer
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.SrcLoc.Prim (SrcLoc# (SrcLoc#))

import GHC.Exts (Int (I#), Int#)
import GHC.Exts qualified as GHC

import Prelude hiding (lex, span, take, takeWhile)

--------------------------------------------------------------------------------

import Data.Parse.Context qualified as Context
import Data.Parse.Error (Error (..), ErrorSort (..))
import Data.Parse.Monad (Parse (P#, unP#))

import Opal.Common.Symbol (Symbol (Symbol))
import Opal.Common.Symbol qualified as Symbol

-- Parse -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runStringParse :: String -> String -> Parse a -> Either Error a
runStringParse source = runBufferParse (FileBuffer.fromString source)
{-# INLINE runStringParse #-}

-- | TODO
--
-- @since 1.0.0
runBufferParse :: FileBuffer -> String -> Parse a -> Either Error a
runBufferParse (FB buf#) name (P# k) =
  let ctx# = Context.makeParseCtx# buf# name
      loc# = SrcLoc# 0# 1# 1#
   in case k ctx# loc# of
        (# _, (# e | #) #) -> Left e
        (# _, (# | x #) #) -> Right x
{-# INLINE runBufferParse #-}

-- Parse Errors ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeError :: ErrorSort -> Parse Error
makeError exn = do 
  Error exn
    <$> asks Context.label
    <*> source'begin
    <*> source'end
    <*> source'posn
    <*> source'span

raise :: ErrorSort -> Parse a
raise exn = throwError =<< makeError exn
{-# INLINE CONLIKE raise #-}

-- Parse Scope -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
label :: String -> Parse a -> Parse a
label name = local (Context.set'label name)
{-# INLINE label #-}

-- | TODO
--
-- @since 1.0.0
scope :: Parse SrcLoc -> Parse SrcLoc -> Parse a -> Parse a
scope pass0 pass1 = scope'begin pass0 . scope'end pass1
{-# INLINE scope #-}

-- | TODO
--
-- @since 1.0.0
scope'begin :: Parse SrcLoc -> Parse a -> Parse a
scope'begin pass lex = do
  loc0 <- get
  loc1 <- pass
  local (Context.set'begin loc1) (put loc0 >> lex)
{-# INLINE scope'begin #-}

-- | TODO
--
-- @since 1.0.0
scope'end :: Parse SrcLoc -> Parse a -> Parse a
scope'end pass lex = do
  loc0 <- get
  loc1 <- pass
  local (Context.set'end loc1) (put loc0 >> lex)
{-# INLINE scope'end #-}

-- Parse State -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
lookahead :: Parse Char
lookahead = do
  pos <- gets (succ . SrcLoc.posn)
  buf <- asks Context.filebuffer
  if pos < FileBuffer.size buf
    then pure (FileBuffer.index buf pos)
    else pure '\NUL'
{-# INLINE lookahead #-}

-- | TODO
--
-- @since 1.0.0
peek :: Parse Char
peek = do
  pos <- gets SrcLoc.posn
  buf <- asks Context.filebuffer
  if pos < FileBuffer.size buf
    then pure (FileBuffer.index buf pos)
    else pure '\NUL'
{-# INLINE peek #-}

-- | TODO
--
-- @since 1.0.0
feed :: Char -> Parse ()
feed chr = do
  pos <- get
  end <- source'end
  if SrcLoc.posn pos < SrcLoc.posn end
    then modify (`SrcLoc.feed` chr)
    else raise ExnFeedEoF
{-# INLINE feed #-}

-- | TODO
--
-- @since 1.0.0
take :: Parse Char
take = do
  chr <- peek
  feed chr $> chr
{-# INLINE take #-}

-- | TODO
--
-- @since 1.0.0
advance :: Parse SrcLoc
advance = do
  pos <- gets SrcLoc.posn
  buf <- asks Context.filebuffer
  when (pos < FileBuffer.size buf) do
    modify (`SrcLoc.feed` FileBuffer.index buf pos)
  get
{-# INLINE advance #-}

-- | TODO
--
-- @since 1.0.0
advanceN :: Int -> Parse SrcLoc
advanceN n = replicateM_ n advance >> get
{-# INLINE advanceN #-}

-- Source Information ----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
source'begin :: Parse SrcLoc
source'begin = asks Context.begin
{-# INLINE source'begin #-}

-- | TODO
--
-- @since 1.0.0
source'end :: Parse SrcLoc
source'end = asks Context.end
{-# INLINE source'end #-}

-- | TODO
--
-- @since 1.0.0
source'posn :: Parse SrcLoc
source'posn = get
{-# INLINE source'posn #-}

-- | TODO
--
-- @since 1.0.0
source'span :: Parse Symbol
source'span = do
  FB buf# <- asks Context.filebuffer
  I# i0# <- SrcLoc.posn <$> source'begin
  I# i1# <- SrcLoc.posn <$> source'end
  GHC.runRW# \st0# ->
    let len# = CharArray.size# buf#
        i1'# = minInt# len# i1#
     in case CharArray.slice# buf# i0# i1'# st0# of
          (# _#, chrs# #) -> pure (Symbol chrs#)
  where
    minInt# :: Int# -> Int# -> Int#
    minInt# x# y# =
      case x# GHC.<# y# of
        1# -> x#
        _ -> y#
{-# INLINE source'span #-}

-- Combinators -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
star :: forall a. Parse a -> Parse [a]
star (P# p#) = run# id
  where
    run# :: ([a] -> [a]) -> Parse [a]
    run# k =
      P# \ctx# loc0# -> case p# ctx# loc0# of
        (# loc1#, (# _ | #) #) -> (# loc1#, (# | k [] #) #)
        (# loc1#, (# | x #) #) ->
          unP# (run# (GHC.oneShot \xs -> xs `seq` k (x : xs))) ctx# loc1#
{-# INLINE star #-}

-- | TODO
--
-- @since 1.0.0
star1 :: Parse a -> Parse [a]
star1 parse = liftA2 (:) parse (star parse)
{-# INLINE star1 #-}

-- | TODO
--
-- @since 1.0.0
between :: Char -> Char -> Parse a -> Parse a
between start end lexer = do
  single start
  scope'begin get do
    tok <- scope'end delimit lexer
    single end
    pure tok
  where
    -- unless (chr == end) do
    --   raise (ExnBalance start end chr)
    -- pure tok

    isdelimiter :: Char -> Bool
    isdelimiter chr = not (chr == start || chr == end)

    -- Scan to the source location of the character @end@ delimiting @start@.
    delimit :: Parse SrcLoc
    delimit = do
      chr <- scan isdelimiter peek
      if chr == start
        then feed chr >> delimit >> single end >> delimit
        else get
{-# INLINE between #-}

-- | TODO
--
-- @since 1.0.0
parens :: Parse a -> Parse a
parens = between '(' ')'
{-# INLINE parens #-}

-- | TODO
--
-- @since 1.0.0
bracks :: Parse a -> Parse a
bracks = between '[' ']'
{-# INLINE bracks #-}

-- Combinators - Alternatives --------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseBranch a
  = BranchFail {-# UNPACK #-} !Error {-# UNPACK #-} !Error
  | BranchLeft a
  | BranchRight a

-- | TODO
--
-- @since 1.0.0
branch :: Parse a -> Parse a -> Parse (ParseBranch a)
branch lex0 lex1 =
  optional lex0 >>= \case
    Right x -> pure (BranchLeft x)
    Left e0 ->
      optional lex1 >>= \case
        Right x -> pure (BranchRight x)
        Left e1 -> pure (BranchFail e0 e1)
{-# INLINE branch #-}

-- | TODO
--
-- @since 1.0.0
attempt :: Parse a -> Parse (Maybe a)
attempt (P# k) =
  P# \ctx# loc0# -> case k ctx# loc0# of
    (# _, (# _ | #) #) -> (# loc0#, (# | Nothing #) #)
    (# loc1#, (# | x #) #) -> (# loc1#, (# | Just x #) #)
{-# INLINE attempt #-}

-- | TODO
--
-- @since 1.0.0
optional :: Parse a -> Parse (Either Error a)
optional (P# k) =
  P# \ctx# loc0# -> case k ctx# loc0# of
    (# _, (# e | #) #) -> (# loc0#, (# | Left e #) #)
    (# loc1#, (# | x #) #) -> (# loc1#, (# | Right x #) #)
{-# INLINE optional #-}

-- | TODO
--
-- @since 1.0.0
alt :: Parse a -> Parse a -> Parse a
alt parse0 parse1 = do
  optional parse0 >>= \case
    Left {} -> parse1
    Right x -> pure x

-- Combinators - Patterns ------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
satisfy :: (Char -> Bool) -> Parse ()
satisfy p = do
  chr <- take
  if p chr
    then feed chr
    else raise (ExnChrMatch Nothing chr)
{-# INLINE satisfy #-}

-- | TODO
--
-- @since 1.0.0
single :: Char -> Parse ()
single match = do
  chr <- peek
  if match == chr
    then feed chr
    else raise (ExnChrMatch (Just match) chr)
{-# INLINE single #-}

-- | TODO
--
-- @since 1.0.0
string :: String -> Parse ()
string match =
  scope get (advanceN $ length match) do
    str <- Symbol.unpack <$> source'span
    if str == match
      then put =<< source'end
      else raise (ExnStrMatch (Just match) str)
{-# INLINE string #-}

-- | TODO
--
-- @since 1.0.0
whitespace :: Parse ()
whitespace = scan isSpace $ pure ()
{-# INLINE whitespace #-}

-- Combinators - Scans ---------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
scan :: (Char -> Bool) -> Parse a -> Parse a
scan match lex =
  scope'begin get do
    loc0 <- source'begin
    loc1 <- source'end
    loop (SrcLoc.diff loc0 loc1) 0
  where
    loop size n
      | n >= size = lex
      | otherwise = do
          chr <- peek
          if match chr
            then feed chr >> loop size (1 + n)
            else lex
{-# INLINE scan #-}

-- | TODO
--
-- @since 1.0.0
scan1 :: (Char -> Bool) -> Parse a -> Parse a
scan1 match lexer = do
  loc0 <- get
  scan match do
    loc1 <- get
    if SrcLoc.diff loc0 loc1 <= 0
      then raise . ExnStrMatch Nothing . Symbol.unpack =<< source'span
      else lexer
{-# INLINE scan1 #-}

-- | TODO
--
-- @since 1.0.0
takeWhile :: (Char -> Bool) -> Parse Symbol
takeWhile match = scope get (scan match get) source'span
{-# INLINE takeWhile #-}

-- | TODO
--
-- @since 1.0.0
takeWhile_ :: (Char -> Bool) -> Parse ()
takeWhile_ match = put =<< scan match get
{-# INLINE takeWhile_ #-}

-- | TODO
--
-- @since 1.0.0
takeWhile1 :: (Char -> Bool) -> Parse Symbol
takeWhile1 match = scope get (scan1 match get) source'span
{-# INLINE takeWhile1 #-}
