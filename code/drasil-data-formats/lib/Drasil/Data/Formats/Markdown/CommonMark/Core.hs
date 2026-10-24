-- | A typed document model for the supported CommonMark subset.
module Drasil.Data.Formats.Markdown.CommonMark.Core
  ( -- * CommonMark document
    CommonMark(..),
    Block(..),
    ListItem(..),
    Inline(..),
    HeadingLevel,
    HTML.HLevel(..),
    URL,
    Source,
    -- * Smart constructors
    mkHeading,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
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
  | -- | A paragraph. Literal spaces and tabs are preserved.
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
  = -- | Plain textual content.
    Plain
      Text -- ^ Text to escape when rendered.
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

-- | Create a heading, rejecting CR, LF, 'SoftBreak' and 'HardBreak', including
-- breaks in nested inline contents. HTML block constructors and @br@ elements
-- are not accepted. Validation does not trim or replace the supplied text.
mkHeading :: HeadingLevel -- ^ Heading level.
          -> [Inline] -- ^ Single-line heading contents.
          -> Either String Block
mkHeading level contents
  | all singleLineInline contents = Right (Heading level contents)
  | otherwise = Left "Headings require inline content without line breaks."

singleLineInline :: Inline -> Bool
singleLineInline (Plain text) = singleLineText text
singleLineInline (Emphasis contents) = all singleLineInline contents
singleLineInline (Strong contents) = all singleLineInline contents
singleLineInline (CodeSpan text) = singleLineText text
singleLineInline (Link url contents) = singleLineText url && all singleLineInline contents
singleLineInline (Image source contents) = singleLineText source && all singleLineInline contents
singleLineInline SoftBreak = False
singleLineInline HardBreak = False
singleLineInline (HTMLInline body) = singleLineHTML body

singleLineText :: Text -> Bool
singleLineText = not . T.any (`elem` ['\r', '\n'])

singleLineHTML :: HTML.HTMLBody -> Bool
singleLineHTML (HTML.RawText text) = singleLineText text
singleLineHTML (HTML.TextFormat _ attrs contents) = singleLineElement attrs contents
singleLineHTML (HTML.Anchor url attrs contents) =
  singleLineText url && singleLineElement attrs contents
singleLineHTML (HTML.Img source alt attrs) =
  all singleLineText [source, alt] && all singleLineAttr attrs
singleLineHTML (HTML.Custom (HTML.CT tag) attrs contents) =
  singleLineText tag && T.toCaseFold tag /= T.pack "br" && singleLineElement attrs contents
singleLineHTML (HTML.Comment text) = singleLineText text
singleLineHTML _ = False

singleLineElement :: [HTML.Attr] -> [HTML.HTMLBody] -> Bool
singleLineElement attrs contents = all singleLineAttr attrs && all singleLineHTML contents

singleLineAttr :: HTML.Attr -> Bool
singleLineAttr (HTML.Attr key value) = all singleLineText [key, value]

-- | A valid CommonMark heading level.
type HeadingLevel = HTML.HLevel

-- | Destination of a link.
type URL = Text

-- | Source of an image.
type Source = Text
