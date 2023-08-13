{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Writer.Doc
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
module Opal.Writer.Doc
  ( -- * Doc
    Doc (..)
    -- ** Basic Operations
  , flattenDoc
    -- ** Combinators
  , empty
  , line
  , space
  , string
  , char
  , double
  , float
  , int
  , word
  , nest
  , indent
  , concat
  , (<+>)
  , (</>)
  , union
  , group
  , sepMap
  , sep
  , hsep
  , vsep
    -- ** Query
  , isLineDoc
    -- * DocStream
  , DocStream (..)
    -- ** Basic Operations
  , layoutDocStream
  , bestFitDoc
    -- ** Query
  , fitsDocStream
  )
where

import Data.String (IsString (..))

import Prelude hiding (concat)

-- Doc -------------------------------------------------------------------------

infixr 6 `DocJoin`
infixl 3 `DocUnion`

-- | The 'Doc' type is the abstract representation for pretty-printer documents.
--
-- @since 1.0.0
data Doc
  = DocNone
  | DocLine
  | DocChar  Char
  | DocText  String
  | DocNest  Int Doc
  | DocJoin  Doc Doc
  | DocUnion Doc Doc
  deriving (Show)

-- | @since 1.0.0
instance IsString Doc where
  fromString = string

-- | @since 1.0.0
instance Monoid Doc where
  mempty = DocNone

-- | @since 1.0.0
instance Semigroup Doc where
  (<>) = DocJoin

-- Doc - Basic Operations ------------------------------------------------------

-- | The function @('flattenDoc' doc)@ "flattens" the given document @doc@ by
-- recursively replacing all 'line' documents with 'space'.
--
-- @since 1.0.0
flattenDoc :: Doc -> Doc
flattenDoc (DocNest  n x) = DocNest n (flattenDoc x)
flattenDoc (DocJoin  x y) = DocJoin (flattenDoc x) (flattenDoc y)
flattenDoc (DocUnion x _) = flattenDoc x
flattenDoc doc
  | isLineDoc doc = char ' '
  | otherwise     = doc

-- Doc - Combinators -----------------------------------------------------------

-- | The @('empty')@ combinator make a new empty document.
--
-- @since 1.0.0
empty :: Doc
empty = DocNone

-- | The @('line')@ combinator make a new document containing a single line
-- break.
--
-- @since 1.0.0
line :: Doc
line = DocLine

-- | The @('space')@ combinator make a new document containing a single space.
--
-- @since 1.0.0
space :: Doc
space = char ' '

-- | The @('string' s)@ combinator creates a new document containing the given
-- 'String'.
--
-- @since 1.0.0
string :: String -> Doc
string ""  = DocNone
string [c] = char c
string str = DocText str

-- | The @('char' s)@ combinator creates a new document containing the given
-- 'Char'.
--
-- @since 1.0.0
char :: Char -> Doc
char c
  | c == '\n' = line
  | otherwise = DocChar c

-- | The @('double' n)@ combinator creates a new document containing the given
-- 'Double' rendered in base 10.
--
-- @since 1.0.0
double :: Double -> Doc
double = string . show -- FIXME: optimize

-- | The @('float' n)@ combinator creates a new document containing the given
-- 'Float' rendered in base 10.
--
-- @since 1.0.0
float :: Float -> Doc
float = string . show -- FIXME: optimize

-- | The @('int' n)@ combinator creates a new document containing the given
-- 'Int' rendered in base 10.
--
-- @since 1.0.0
int :: Int -> Doc
int = string . show -- FIXME: optimize

-- | The @('word' n)@ combinator creates a new document containing the given
-- 'Word' rendered in base 10.
--
-- @since 1.0.0
word :: Word -> Doc
word = string . show -- FIXME: optimize

-- | The @('nest' n x)@ combinator creates a new document adjusts the current
-- indentation level after each line break in the given document @x@ by @n@
-- spaces.
--
-- @since 1.0.0
nest :: Int -> Doc -> Doc
nest = DocNest

-- | The @('indent' n x)@ adjusts the current indentation level by @n@, similar
-- to  the 'nest' combinator, but also inserts a line break just before the
-- given document @x@.
--
-- @since 1.0.0
indent :: Int -> Doc -> Doc
indent n x = nest n (line <> x)

infixr 6 `concat`

-- | The @('concat' x y)@ combinator creates a new document that is the
-- concatenation of the two documents @x@ and @y@.
--
-- @since 1.0.0
concat :: Doc -> Doc -> Doc
concat DocNone y       = y
concat x       DocNone = x
concat x       y       = DocJoin x y

infixr 6 <+>

-- | The "horizontal contcatenation" operator @(x '<+>' y)@ creates a new
-- document that is the concatenation of the two documents @x@ and @y@ seperated
-- by a space.
--
-- @since 1.0.0
(<+>) :: Doc -> Doc -> Doc
DocNone <+> y       = y
x       <+> DocNone = x
x       <+> y       = x <> space <> y

infixr 6 </>

-- | The "vertical contcatenation" operator @(x '<+>' y)@ creates a new document
-- that is the concatenation of the two documents @x@ and @y@ seperated by a
-- line break.
--
-- @since 1.0.0
(</>) :: Doc -> Doc -> Doc
x </> y = x <> line <> y

infixl 3 `union`

-- | The @('union' x y)@ combinator creates a new document that will render the
-- document @x@ a single line. If @x@ exceeds the maximum column width when
-- being rendered, then the alternate document @y@ will be rendered instead.
--
-- @since 1.0.0
union :: Doc -> Doc -> Doc
union = DocUnion

-- | The @('group' x)@ combinator creates a new document that will render the
-- document @x@ a single line. If @x@ exceeds the maximum column width when
-- being rendered, then any line breaks in the document @x@ will be preserved.
--
-- @since 1.0.0
group :: Doc -> Doc
group x = flattenDoc x `union` x

-- | The @('sep' s xs)@ combinator creates a new document that is the
-- concatenation of all documents @xs@ seperated by @s@.
--
-- @since 1.0.0
sepMap :: (a -> Doc) -> Doc -> [a] -> Doc
sepMap _ _ []       = empty
sepMap f s (x : xs) = f x <> run xs
  where
    run []       = empty
    run (y : ys) = s <> f y <> run ys

-- | The @('sep' s xs)@ combinator creates a new document that is the
-- concatenation of all documents @xs@ seperated by @s@.
--
-- @since 1.0.0
sep :: Doc -> [Doc] -> Doc
sep = sepMap id

-- | The @('hsep' s xs)@ combinator creates a new document that is the
-- concatenation of all documents @xs@ seperated by a space.
--
-- @since 1.0.0
hsep :: [Doc] -> Doc
hsep = sep space

-- | The @('vsep' s xs)@ combinator creates a new document that is the
-- concatenation of all documents @xs@ seperated by a line break.
--
-- @since 1.0.0
vsep :: [Doc] -> Doc
vsep = sep line

-- Doc - Query -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
isLineDoc :: Doc -> Bool
isLineDoc DocLine          = True
isLineDoc (DocChar '\n')   = True
isLineDoc (DocText ['\n']) = True
isLineDoc _                = False

-- DocStream -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DocStream
  = StreamDone
  | StreamText String DocStream
  | StreamLine Int DocStream

-- | @since 1.0.0
instance IsString DocStream where
  fromString s = StreamText s StreamDone

-- | @since 1.0.0
instance Monoid DocStream where
  mempty = StreamDone

-- | @since 1.0.0
instance Semigroup DocStream where
  StreamDone     <> y = y
  StreamText s x <> y = StreamText s (x <> y)
  StreamLine n x <> y = StreamLine n (x <> y)

-- DocStream - Basic Operations ------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
layoutDocStream :: DocStream -> String
layoutDocStream StreamDone       = ""
layoutDocStream (StreamText s x) = s ++ layoutDocStream x
layoutDocStream (StreamLine n x) = '\n' : replicate n ' ' ++ layoutDocStream x

-- | TODO: docs
--
-- @since 1.0.0
bestFitDoc :: Int -> Int -> Doc -> DocStream
bestFitDoc w k0 doc = run k0 [(0, doc)]
  where
    run :: Int -> [(Int, Doc)] -> DocStream
    run _ []                       = StreamDone
    run k ((_, DocNone)      : xs) = run k xs
    run _ ((i, DocLine)      : xs) = StreamLine i (run i xs)
    run k ((_, DocChar  c)   : xs) = StreamText [c] (run (k + 1) xs)
    run k ((_, DocText  s)   : xs) = StreamText s (run (k + length s) xs)
    run k ((i, DocNest  n x) : xs) = run k ((i + n, x) : xs)
    run k ((i, DocJoin  x y) : xs) = run k ((i, x) : (i, y) : xs)
    run k ((i, DocUnion x y) : xs) =
      let x' :: DocStream
          x' = run k ((i, x) : xs)
       in if fitsDocStream (w - k) x'
            then x'
            else run k ((i, y) : xs)

-- DocStream - Query -----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
fitsDocStream :: Int -> DocStream -> Bool
fitsDocStream w _ | w < 0        = False
fitsDocStream _ StreamDone       = True
fitsDocStream w (StreamText s x) = fitsDocStream (w - length s) x
fitsDocStream _ (StreamLine _ _) = True
