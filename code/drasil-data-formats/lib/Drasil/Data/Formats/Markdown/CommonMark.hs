-- | Typed CommonMark documents and rendering with structured HTML fallbacks.
--
-- Build a CommonMark document from blocks, then apply 'renderCommonMark' with
-- 'defaultCommonMarkRO' or customized rendering options. The resulting
-- prettyprinter document can be written using Drasil's file-handling library.
--
-- With @OverloadedStrings@ enabled:
--
-- @
-- renderCommonMark defaultCommonMarkRO
--   (CommonMark [Heading H1 [Plain "Example"], Paragraph [Plain "Hello"]])
-- @
module Drasil.Data.Formats.Markdown.CommonMark
  ( -- * Document model
    module Drasil.Data.Formats.Markdown.CommonMark.Core,
    -- * Rendering
    renderCommonMark,
    CommonMarkRenderOptions (..),
    EmphasisDelimiter (..),
    defaultCommonMarkRO,
  )
where

import Drasil.Data.Formats.Markdown.CommonMark.Core
import Drasil.Data.Formats.Markdown.CommonMark.Render
  ( CommonMarkRenderOptions (..), EmphasisDelimiter (..),
    defaultCommonMarkRO, renderCommonMark )
