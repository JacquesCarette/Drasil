-- | Typed CommonMark documents and rendering with structured HTML fallbacks.
--
-- Build a CommonMark document from blocks, then apply 'renderCommonMark' with
-- 'defaultCommonMarkRO' or customized rendering options. The resulting
-- prettyprinter document can be written using Drasil's file-handling library.
--
-- Create headings with 'mkHeading'; invalid contents return an error.
-- The unchecked heading constructor is not part of this public API.
--
-- With @OverloadedStrings@ enabled:
--
-- @
-- case mkHeading H1 [Plain "Example"] of
--   Left message -> error message
--   Right title -> renderCommonMark defaultCommonMarkRO
--     (CommonMark [title, Paragraph [Plain "Hello"]])
-- @
--
-- Select tilde fences and specify the language separately for each code block:
--
-- @
-- renderCommonMark (defaultCommonMarkRO {codeFenceStyle = TildeFence})
--   (CommonMark
--     [CodeBlock (Just "python") "x = 1", CodeBlock (Just "haskell") "x = 1"])
-- @
module Drasil.Data.Formats.Markdown.CommonMark
  ( -- * Document model
    CommonMark (..),
    Block (Paragraph, BlockQuote, CodeBlock, OrderedList, UnorderedList, ThematicBreak, HTMLBlock),
    ListItem (..),
    Inline (..),
    HeadingLevel,
    HLevel (..),
    URL,
    Source,
    mkHeading,
    -- * Rendering
    renderCommonMark,
    CommonMarkRenderOptions (..),
    EmphasisDelimiter (..),
    CodeFenceStyle (..),
    defaultCommonMarkRO,
  )
where

import Drasil.Data.Formats.Markdown.CommonMark.Core
import Drasil.Data.Formats.Markdown.CommonMark.Render
  ( CodeFenceStyle (..), CommonMarkRenderOptions (..), EmphasisDelimiter (..),
    defaultCommonMarkRO, renderCommonMark )
