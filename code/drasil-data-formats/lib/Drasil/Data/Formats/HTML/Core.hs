module Drasil.Data.Formats.HTML.Core
  ( -- * JSON
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), CustomTag(..), customTag,
    Format(..), HLevel(..), Row(..), Cell(..), LItem(..), DItem(..), ListType(..),
    Attr(..), bold, emphasis, subscript, superscript, span, figureImage
  )
where

import Prelude hiding (span)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

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
  | Custom CustomTag [Attr] [HTMLBody]
  | Comment Text
  deriving (Show, Eq)
-- TODO: Support more tags
-- https://www.w3schools.com/tags/default.asp

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

-- | A 'CustomTag' is either (a) an ill-supported HTML-spec. node (ill-supported
-- by 'HTMLBody', that is) or (b) a purely custom one.
newtype CustomTag = CT Text
  deriving (Show, Eq, Ord)

data TagType = Standard | Void
  deriving (Show, Eq)

customTag :: Text -> CustomTag
customTag t
  -- | Tag names are used within element start tags and end tags to give the
  -- element’s name. HTML elements all have names that only use characters in
  -- the range 0–9, a–z, and A–Z.
  | isSanitary t = CT t
  | otherwise    = error "Bad custom tag name"

isSanitary :: Text -> Bool
isSanitary t = not (T.null t) && isAsciiLetter (T.head t) && T.all isAllowedChar t
  where
    -- The first character must be a letter
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c
    isAllowedChar c = isAsciiLetter c || isDigit c || c == '-'

-- | Smart Constructors

bold :: [Attr] -> Text -> HTMLBody
bold attrs txt = TextFormat Bold attrs [RawText txt]

emphasis :: [Attr] -> Text -> HTMLBody
emphasis attrs txt = TextFormat Emphasis attrs [RawText txt]

subscript :: [Attr] -> Text -> HTMLBody
subscript attrs txt = TextFormat Subscript attrs [RawText txt]

superscript :: [Attr] -> Text -> HTMLBody
superscript attrs txt = TextFormat Superscript attrs [RawText txt]

span :: [Attr] -> Text -> HTMLBody
span attrs txt = TextFormat Span attrs [RawText txt]

figureImage :: [Attr] -> File -> Text -> Text -> HTMLBody
figureImage attrs src altText captionTxt =
  Figure attrs [Img src altText [], FigCaption [] [RawText captionTxt]]
