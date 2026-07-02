module Drasil.Data.Formats.HTML.Core
  ( -- * JSON
    HTML(..), HTMLBody(..), HTMLHead(..), Format(..), HLevel(..), Row(..),
    Cell(..), LItem(..), DItem(..), ListType(..), Attribute(..)
  )
where

import Data.Text (Text)

-- | HTML attributes for tags in the format key="value" and booleanAttribute
data Attribute =
    Attr Text Text
  | BoolAttr Text
  deriving (Show, Eq)

data HTML = HTML [HTMLHead] [HTMLBody]
  deriving (Show, Eq)

-- | Head elements
data HTMLHead =
    Script [Attribute] Text
  | Title Text
  | Meta [Attribute]
  | Link [Attribute]
  deriving (Show, Eq)

-- | Body elements
data HTMLBody =
  Div [Attribute] [HTMLBody]
  | Paragraph [Attribute] [HTMLBody]
  | TextFormat Format [Attribute] [HTMLBody]
  | Heading HLevel [Attribute] [HTMLBody]
  | List ListType [Attribute] [LItem]
  | Table [Attribute] [Row]
  | DescriptionList [Attribute] [DItem]
  | Anchor URL [Attribute] [HTMLBody]
  | Figure [Attribute] [HTMLBody]
  | FigCaption [Attribute] [HTMLBody]
  | Img File [Attribute]
    -- | Raw unescaped text content
  | RawText Text
    -- | Custom tag that wraps children elements
  | CustomTag Text [Attribute] [HTMLBody]
    -- | Empty/void tags cannot contain children
  | EmptyCustomTag Text [Attribute]
  deriving (Show, Eq)
-- TODO: Support more tags
-- https://www.w3schools.com/tags/default.asp

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
data LItem = LItem [Attribute] [HTMLBody]
  deriving (Show, Eq)

-- | Description list elements
data DItem =
    DTerm [Attribute] [HTMLBody]
  | DDetails [Attribute] [HTMLBody]
  deriving (Show, Eq)

-- | Table structure
data Row = Row [Attribute] [Cell]
  deriving (Show, Eq)

data Cell =
    THeader [Attribute] [HTMLBody]
  | TData [Attribute] [HTMLBody]
  deriving (Show, Eq)
