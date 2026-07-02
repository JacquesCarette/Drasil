module Drasil.Data.Formats.HTML.Core
  ( -- * JSON
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), Format(..), HLevel(..),
    Row(..), Cell(..), LItem(..), DItem(..), ListType(..), Attr(..),
    boldText, emphasisText, subscriptText, superscriptText, spanText, figureImage
  )
where

import Data.Text (Text)

-- | HTML Attrs for tags in the format key="value"
data Attr = Attr Text Text
  deriving (Show, Eq)

data HTML = HTML [HTMLHead] [HTMLBody]
  deriving (Show, Eq)

-- | Head elements
data HTMLHead =
    Script [Attr] Text
  | Title Text
  | Meta [Attr]
  | Link Relation File [Attr]
  deriving (Show, Eq)

-- | Body elements
data HTMLBody =
  Div [Attr] [HTMLBody]
  | Paragraph [Attr] [HTMLBody]
  | TextFormat Format [Attr] [HTMLBody]
  | Heading HLevel [Attr] [HTMLBody]
  | List ListType [Attr] [LItem]
  | Table [Attr] [Row]
  | DescriptionList [Attr] [DItem]
  | Anchor URL [Attr] [HTMLBody]
  | Figure [Attr] [HTMLBody]
  | FigCaption [Attr] [HTMLBody]
  | Img File Text [Attr]
  | RawText Text
  | CustomTag Text TagType [Attr] [HTMLBody]
  | Comment Text
  deriving (Show, Eq)
-- TODO: Support more tags
-- https://www.w3schools.com/tags/default.asp

data TagType = Standard | Void
  deriving (Show, Eq)

type Relation = Text
-- | Target link
type URL = Text
-- | File name or file path.
type File = Text

-- | Text format
data Format = Bold | Emphasis | Subscript | Superscript | Span
  deriving (Show, Eq)

-- | Heading level
data HLevel = H1 | H2 | H3 | H4 | H5 | H6
  deriving (Show, Eq)

-- | List type
data ListType = Ordered | Unordered
  deriving (Show, Eq)

-- | Ordered/unordered list structure
data LItem = LItem [Attr] [HTMLBody]
  deriving (Show, Eq)

-- | Description list elements
data DItem =
    DTerm [Attr] [HTMLBody]
  | DDetails [Attr] [HTMLBody]
  deriving (Show, Eq)

-- | Table structure
data Row = Row [Attr] [Cell]
  deriving (Show, Eq)

data Cell =
    THeader [Attr] [HTMLBody]
  | TData [Attr] [HTMLBody]
  deriving (Show, Eq)

-- | Smart Constructors

boldText :: [Attr] -> Text -> HTMLBody
boldText attrs txt = TextFormat Bold attrs [RawText txt]

emphasisText :: [Attr] -> Text -> HTMLBody
emphasisText attrs txt = TextFormat Emphasis attrs [RawText txt]

subscriptText :: [Attr] -> Text -> HTMLBody
subscriptText attrs txt = TextFormat Subscript attrs [RawText txt]

superscriptText :: [Attr] -> Text -> HTMLBody
superscriptText attrs txt = TextFormat Superscript attrs [RawText txt]

spanText :: [Attr] -> Text -> HTMLBody
spanText attrs txt = TextFormat Span attrs [RawText txt]

figureImage :: File -> Text -> Text -> HTMLBody
figureImage src altText captionTxt =
  Figure [] [ Img src altText [], FigCaption [] [RawText captionTxt]]
