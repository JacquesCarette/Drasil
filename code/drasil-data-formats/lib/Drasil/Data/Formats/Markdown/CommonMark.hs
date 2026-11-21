-- | Typed CommonMark documents and rendering with structured HTML fallbacks.
--
-- Build a document with 'mkCommonMark', then apply 'renderCommonMark' with
-- 'defaultCommonMarkRO' or customized rendering options. The resulting
-- prettyprinter document can be written using Drasil's file-handling library.
--
-- Plain text must not contain CR or LF; use 'SoftBreak', 'HardBreak' or
-- separate paragraphs. Create single-line headings with 'mkHeading'.
-- Both smart constructors return errors for invalid input; unchecked document
-- and heading constructors are not part of this public API.
--
-- With @OverloadedStrings@ enabled:
--
-- @
-- document = do
--   title <- mkHeading H1 [Plain "Example"]
--   mkCommonMark [title, Paragraph [Plain "Hello"]]
-- case document of
--   Left message -> error message
--   Right validDocument -> renderCommonMark defaultCommonMarkRO validDocument
-- @
--
-- Select tilde fences and specify the language separately for each code block:
--
-- @
-- case mkCommonMark
--   [CodeBlock (Just "python") "x = 1", CodeBlock (Just "haskell") "x = 1"] of
--   Left message -> error message
--   Right document ->
--     renderCommonMark (defaultCommonMarkRO {codeFenceStyle = TildeFence}) document
-- @
module Drasil.Data.Formats.Markdown.CommonMark
  ( -- * Document model
    CommonMark,
    Block (Paragraph, BlockQuote, CodeBlock, OrderedList, UnorderedList, ThematicBreak, HTMLBlock),
    ListItem (..),
    Inline (..),
    HeadingLevel (..),
    URL,
    Source,
    mkCommonMark,
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
