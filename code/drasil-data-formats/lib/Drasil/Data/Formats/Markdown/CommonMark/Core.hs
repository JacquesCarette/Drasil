-- | A typed document model for the supported CommonMark subset.
module Drasil.Data.Formats.Markdown.CommonMark.Core
  ( -- * CommonMark document
    CommonMark(..),
    Block(..),
    ListItem(..),
    Inline(..),
    HeadingLevel(..),
    URL,
    Source,
    -- * Smart constructors
    mkCommonMark,
    mkHeading,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Drasil.Data.Formats.HTML.Core (HeadingLevel(..))
import Drasil.Data.Formats.HTML.Core qualified as HTML

-- | A complete CommonMark document.
newtype CommonMark
  = -- | Construct a document from top-level blocks.
    CommonMark
      [Block] -- ^ Top-level blocks in source order.
  deriving (Eq, Show)

-- | A block-level CommonMark node.
data Block
  = -- | A heading. Use 'mkHeading' to validate its contents.
    Heading
      HeadingLevel -- ^ Heading level.
      [Inline] -- ^ Heading contents.
  | -- | A paragraph. Ordinary edge whitespace follows CommonMark rules.
    Paragraph
      [Inline] -- ^ Paragraph contents.
  | -- | A block quote.
    BlockQuote
      [Block] -- ^ Quoted block contents.
  | -- | A fenced code block.
    CodeBlock
      (Maybe Text) -- ^ Optional info string; its first word usually names the code language.
      Text -- ^ Literal code contents.
  | -- | An ordered list.
    OrderedList
      [ListItem] -- ^ Items in source order.
  | -- | An unordered list.
    UnorderedList
      [ListItem] -- ^ Items in source order.
  | -- | A thematic break.
    ThematicBreak
  | -- | Structured HTML rendered as a block fallback.
    HTMLBlock
      [HTML.HTMLBody] -- ^ HTML body nodes in source order.
  deriving (Eq, Show)

-- | A list item containing zero or more blocks.
newtype ListItem
  = -- | Construct a list item from block contents.
    ListItem
      [Block] -- ^ Blocks belonging to this item.
  deriving (Eq, Show)

-- | An inline CommonMark node.
data Inline
  = -- | Plain text without CR or LF. Use explicit break nodes for newlines.
    Plain
      Text -- ^ Single-line text to escape when rendered.
  | -- | Emphasized contents.
    Emphasis
      [Inline] -- ^ Inline contents to emphasize.
  | -- | Strongly emphasized contents.
    Strong
      [Inline] -- ^ Inline contents to strongly emphasize.
  | -- | Inline code. Empty contents are retained using an HTML fallback.
    CodeSpan
      Text -- ^ Literal code contents.
  | -- | A hyperlink.
    Link
      URL -- ^ Link destination.
      [Inline] -- ^ Link label.
  | -- | An image.
    Image
      Source -- ^ Image source.
      [Inline] -- ^ Image description.
  | -- | A source newline that may render as a space.
    SoftBreak
  | -- | A forced line break.
    HardBreak
  | -- | Structured HTML rendered as an inline fallback.
    HTMLInline
      HTML.HTMLBody -- ^ HTML body node placed inline.
  deriving (Eq, Show)

-- | Build a document, rejecting CR and LF in every 'Plain' node.
-- Use 'SoftBreak', 'HardBreak' or separate paragraphs instead.
-- Code, HTML and link destinations keep their existing newline rules.
mkCommonMark :: [Block] -> Either String CommonMark
mkCommonMark blocks
  | any blockHasPlainLineBreak blocks =
      Left "Plain text cannot contain CR or LF; use SoftBreak, HardBreak or separate Paragraphs."
  | otherwise = Right (CommonMark blocks)
  where
    blockHasPlainLineBreak (Heading _ contents) = any inlineHasPlainLineBreak contents
    blockHasPlainLineBreak (Paragraph contents) = any inlineHasPlainLineBreak contents
    blockHasPlainLineBreak (BlockQuote contents) = any blockHasPlainLineBreak contents
    blockHasPlainLineBreak (OrderedList items) = any itemHasPlainLineBreak items
    blockHasPlainLineBreak (UnorderedList items) = any itemHasPlainLineBreak items
    blockHasPlainLineBreak (CodeBlock _ _) = False
    blockHasPlainLineBreak ThematicBreak = False
    blockHasPlainLineBreak (HTMLBlock _) = False

    itemHasPlainLineBreak (ListItem contents) = any blockHasPlainLineBreak contents

    inlineHasPlainLineBreak (Plain text) = not (isSingleLineText text)
    inlineHasPlainLineBreak (Emphasis contents) = any inlineHasPlainLineBreak contents
    inlineHasPlainLineBreak (Strong contents) = any inlineHasPlainLineBreak contents
    inlineHasPlainLineBreak (Link _ contents) = any inlineHasPlainLineBreak contents
    inlineHasPlainLineBreak (Image _ contents) = any inlineHasPlainLineBreak contents
    inlineHasPlainLineBreak (CodeSpan _) = False
    inlineHasPlainLineBreak (HTMLInline _) = False
    inlineHasPlainLineBreak SoftBreak = False
    inlineHasPlainLineBreak HardBreak = False

-- | Create a heading, rejecting CR, LF, 'SoftBreak' and 'HardBreak', including
-- breaks in nested inline contents. HTML block constructors and @br@ elements
-- are not accepted. Validation does not trim or replace the supplied text.
mkHeading :: HeadingLevel -- ^ Heading level.
          -> [Inline] -- ^ Single-line heading contents.
          -> Either String Block
mkHeading level contents
  | all isSingleLineInline contents = Right (Heading level contents)
  | otherwise = Left "Headings require inline content without line breaks."

isSingleLineInline :: Inline -> Bool
isSingleLineInline (Plain text) = isSingleLineText text
isSingleLineInline (Emphasis contents) = all isSingleLineInline contents
isSingleLineInline (Strong contents) = all isSingleLineInline contents
isSingleLineInline (CodeSpan text) = isSingleLineText text
isSingleLineInline (Link url contents) = isSingleLineText url && all isSingleLineInline contents
isSingleLineInline (Image source contents) = isSingleLineText source && all isSingleLineInline contents
isSingleLineInline SoftBreak = False
isSingleLineInline HardBreak = False
isSingleLineInline (HTMLInline body) = isSingleLineHtml body

isSingleLineText :: Text -> Bool
isSingleLineText = not . T.any (`elem` ['\r', '\n'])

isSingleLineHtml :: HTML.HTMLBody -> Bool
isSingleLineHtml (HTML.RawText text) = isSingleLineText text
isSingleLineHtml (HTML.TextFormat _ attrs contents) = isSingleLineElement attrs contents
isSingleLineHtml (HTML.Anchor url attrs contents) =
  isSingleLineText url && isSingleLineElement attrs contents
isSingleLineHtml (HTML.Img source alt attrs) =
  all isSingleLineText [source, alt] && all isSingleLineAttr attrs
isSingleLineHtml (HTML.Custom (HTML.CT tag) attrs contents) =
  isSingleLineText tag && T.toCaseFold tag /= T.pack "br" && isSingleLineElement attrs contents
isSingleLineHtml (HTML.Comment text) = isSingleLineText text
isSingleLineHtml _ = False

isSingleLineElement :: [HTML.Attr] -> [HTML.HTMLBody] -> Bool
isSingleLineElement attrs contents = all isSingleLineAttr attrs && all isSingleLineHtml contents

isSingleLineAttr :: HTML.Attr -> Bool
isSingleLineAttr (HTML.Attr key value) = all isSingleLineText [key, value]

-- | Destination of a link.
type URL = Text

-- | Source of an image.
type Source = Text
