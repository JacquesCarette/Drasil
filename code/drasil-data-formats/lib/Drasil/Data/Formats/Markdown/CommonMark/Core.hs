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
  )
where

import Data.Text (Text)
import Drasil.Data.Formats.HTML.Core qualified as HTML

-- | A complete CommonMark document.
newtype CommonMark
  = -- | Construct a document from top-level blocks.
    CommonMark
      [Block] -- ^ Top-level blocks in source order.
  deriving (Eq, Show)

-- | A block-level CommonMark node.
data Block
  = -- | A heading.
    Heading
      HeadingLevel -- ^ Heading level.
      [Inline] -- ^ Heading contents.
  | -- | A paragraph.
    Paragraph
      [Inline] -- ^ Paragraph contents.
  | -- | A block quote.
    BlockQuote
      [Block] -- ^ Quoted block contents.
  | -- | A fenced code block.
    CodeBlock
      (Maybe Text) -- ^ Optional CommonMark info string.
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

-- | A list item containing one or more blocks.
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
  | -- | Inline code.
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

-- | A valid CommonMark heading level.
data HeadingLevel
  = H1 -- ^ Level-one heading.
  | H2 -- ^ Level-two heading.
  | H3 -- ^ Level-three heading.
  | H4 -- ^ Level-four heading.
  | H5 -- ^ Level-five heading.
  | H6 -- ^ Level-six heading.
  deriving (Eq, Show, Enum, Bounded)

-- | Destination of a link.
type URL = Text

-- | Source of an image.
type Source = Text
