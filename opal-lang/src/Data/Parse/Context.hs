{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Parse.Context
  ( -- * Parser Contexts
    ParseCtx (ParseCtx),
    makeParseCtx,
    filebuffer,
    label,
    set'label,
    begin,
    set'begin,
    end,
    set'end,

    -- * TODO
    ParseCtx# (ParseCtx#),
    makeParseCtx#,
    filebuffer#,
    label#,
    set'label#,
    begin#,
    set'begin#,
    end#,
    set'end#,
  )
where

import Data.CharArray.Prim (CharArray#)
import Data.CharArray.Prim qualified as CharArray
import Data.IO.FileBuffer (FileBuffer (FB))
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc.Prim (SrcLoc# (SrcLoc#))
import Data.SrcLoc qualified as SrcLoc
import Data.SrcLoc.Prim qualified as SrcLoc

import qualified GHC.Exts as GHC
import GHC.Exts (Int#)

--------------------------------------------------------------------------------


-- Parser Contexts -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseCtx = ParseCtx ParseCtx#

-- | TODO 
--
-- @since 1.0.0
makeParseCtx :: FileBuffer -> String -> ParseCtx
makeParseCtx (FB buf#) name = ParseCtx (makeParseCtx# buf# name)
{-# INLINE makeParseCtx #-}

-- | TODO
--
-- @since 1.0.0
filebuffer :: ParseCtx -> FileBuffer
filebuffer (ParseCtx ctx#) = FB (filebuffer# ctx#)
{-# INLINE filebuffer #-}

-- | TODO
--
-- @since 1.0.0
label :: ParseCtx -> String
label (ParseCtx ctx#) = label# ctx#
{-# INLINE label #-}

-- | TODO
--
-- @since 1.0.0
set'label :: String -> ParseCtx -> ParseCtx 
set'label lbl (ParseCtx ctx#) = ParseCtx (set'label# lbl ctx#)
{-# INLINE set'label #-}

-- | TODO
--
-- @since 1.0.0
begin :: ParseCtx -> SrcLoc
begin (ParseCtx ctx#) = SrcLoc.box (begin# ctx#)
{-# INLINE begin #-}

-- | TODO
--
-- @since 1.0.0
set'begin :: SrcLoc -> ParseCtx -> ParseCtx
set'begin loc (ParseCtx ctx#) = ParseCtx (set'begin# (SrcLoc.unbox loc) ctx#)
{-# INLINE set'begin #-}

-- | TODO
--
-- @since 1.0.0
end :: ParseCtx -> SrcLoc
end (ParseCtx ctx#) = SrcLoc.box (end# ctx#)
{-# INLINE end #-}

-- | TODO
--
-- @since 1.0.0
set'end :: SrcLoc -> ParseCtx -> ParseCtx
set'end loc (ParseCtx ctx#) = ParseCtx (set'end# (SrcLoc.unbox loc) ctx#)
{-# INLINE set'end #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype ParseCtx# = Ctx# (# CharArray#, String, SrcLoc#, SrcLoc# #)

pattern ParseCtx# :: CharArray# -> String -> SrcLoc# -> SrcLoc# -> ParseCtx#
pattern ParseCtx# buf# lbl begin# end# = Ctx# (# buf#, lbl, begin#, end# #)

{-# COMPLETE ParseCtx# #-}

-- | TODO 
--
-- @since 1.0.0
makeParseCtx# :: CharArray# -> String -> ParseCtx#
makeParseCtx# buf# name = 
  let loc# :: SrcLoc# 
      loc# = SrcLoc# 0# 1# 1#
   in ParseCtx# buf# name loc# (loop# loc#)
  where 
    size# :: Int# 
    size# = CharArray.size# buf# 

    loop# :: SrcLoc# -> SrcLoc#
    loop# loc# = 
      let posn# :: Int#
          posn# = SrcLoc.posn# loc#
       in case posn# GHC.<# size# of 
            1# -> loop# (SrcLoc.feed# loc# (CharArray.unsafeIndex# buf# posn#))
            _ -> loc#

-- | TODO
--
-- @since 1.0.0
filebuffer# :: ParseCtx# -> CharArray#
filebuffer# (ParseCtx# buf# _ _ _) = buf#

-- | TODO
--
-- @since 1.0.0
label# :: ParseCtx# -> String
label# (ParseCtx# _ lbl# _ _) = lbl#

-- | TODO
--
-- @since 1.0.0
set'label# :: String -> ParseCtx# -> ParseCtx#
set'label# lbl# (ParseCtx# buf# _ loc0# loc1#) = ParseCtx# buf# lbl# loc0# loc1#
{-# INLINE set'label# #-}

-- | TODO
--
-- @since 1.0.0
begin# :: ParseCtx# -> SrcLoc#
begin# (ParseCtx# _ _ loc# _) = loc#

-- | TODO
--
-- @since 1.0.0
set'begin# :: SrcLoc# -> ParseCtx# -> ParseCtx#
set'begin# loc0# (ParseCtx# buf# lbl# _ loc1#) = ParseCtx# buf# lbl# loc0# loc1#

-- | TODO
--
-- @since 1.0.0
end# :: ParseCtx# -> SrcLoc#
end# (ParseCtx# _ _ _ loc#) = loc#

-- | TODO
--
-- @since 1.0.0
set'end# :: SrcLoc# -> ParseCtx# -> ParseCtx#
set'end# loc1# (ParseCtx# buf# lbl# loc0# _) = ParseCtx# buf# lbl# loc0# loc1#
