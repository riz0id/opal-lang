{-# LANGUAGE RankNTypes #-}

module Opal.Print
  ( -- * TODO
    render,
    layoutOptions,

    -- * TODO
    layoutFormatted,

    -- * TODO
    pprSyntax,
  )
where

import Prettyprinter
  ( Doc,
    LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine),
    SimpleDocStream,
  )
import Prettyprinter qualified as Print
import Prettyprinter.Render.String (renderString)

--------------------------------------------------------------------------------

import Opal.Expand.Syntax (Syntax)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
render :: Doc ann -> String
render = 
  renderString 
    . layoutFormatted layoutOptions 
    . Print.fuse Print.Deep

-- | TODO
--
-- @since 1.0.0
layoutOptions :: LayoutOptions
layoutOptions = LayoutOptions (AvailablePerLine 80 1.0)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
layoutFormatted :: LayoutOptions -> Doc ann -> SimpleDocStream ann
layoutFormatted = Print.layoutPretty 

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pprSyntax :: Syntax -> Doc ann
pprSyntax stx =
  let doc = Print.pretty stx
   in doc