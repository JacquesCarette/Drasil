{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.HTML.Core
  ( -- * HTML
    -- ** AST
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), CustomTag(..), Attr(..),
    Format(..), HLevel(..), Row(..), Cell(..), LItem(..), DItem(..), ListType(..),
    -- * Smart Constructors
    attr, id_, class_, rawText, rawText', customTag,
    bold, bold_, emphasis, emphasis_, subscript, subscript_, superscript, superscript_,
    span, span_, toHLevel, figureImage, inlineScript, externalScript, stylesheet
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (span)

-- | HTML Attrs for tags in the format key="value"
data Attr = Attr
  Text -- ^ Key
  Text -- ^ Value
  deriving (Show, Eq)

data HTML = HTML [HTMLHead] [HTMLBody]
  deriving (Show, Eq)

-- | Head elements
data HTMLHead
  = Script [Attr] Text
  | Title Text
  | Meta [Attr]
  | Link Relation File [Attr]
  deriving (Show, Eq)

-- | Body elements
data HTMLBody
  = Div [Attr] [HTMLBody]
  | Paragraph [Attr] [HTMLBody]
  | TextFormat Format [Attr] [HTMLBody]
  | Heading HLevel [Attr] [HTMLBody]
  | List ListType [Attr] [LItem]
  | Section [Attr] [HTMLBody]
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

instance IsString HTMLBody where
  fromString = RawText . T.pack

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
  deriving (Show, Eq, Enum, Bounded)

-- * Conversions

-- | Converts a 0-indexed integer to a heading level (capped at H6).
toHLevel :: Int -> HLevel
toHLevel 0 = H1
toHLevel 1 = H2
toHLevel 2 = H3
toHLevel 3 = H4
toHLevel 4 = H5
toHLevel _ = H6

-- | List type
data ListType = Ordered | Unordered
  deriving (Show, Eq)

-- | Ordered/unordered list structure
data LItem = LItem [Attr] [HTMLBody]
  deriving (Show, Eq)

-- | Description list elements
data DItem
  = DTerm [Attr] [HTMLBody]
  | DDetails [Attr] [HTMLBody]
  deriving (Show, Eq)

-- | Table structure
data Row = Row [Attr] [Cell]
  deriving (Show, Eq)

data Cell
  = THeader [Attr] [HTMLBody]
  | TData [Attr] [HTMLBody]
  deriving (Show, Eq)

-- | A 'CustomTag' is either (a) an ill-supported HTML-spec. node (ill-supported
-- by 'HTMLBody', that is) or (b) a purely custom one.
newtype CustomTag = CT Text
  deriving (Show, Eq, Ord)

data TagType = Standard | Void
  deriving (Show, Eq)

-- | Tag names are used within element start tags and end tags to give the
-- element’s name. HTML elements all have names that only use characters in
-- the range 0–9, a–z, and A–Z.
customTag :: Text -> CustomTag
customTag t
  | isSanitary t = CT t
  | otherwise = error $ "Bad custom tag name: " <> T.unpack t

isSanitary :: Text -> Bool
isSanitary t = not (T.null t) && isAsciiLetter (T.head t) && T.all isAllowedChar t
  where
    -- The first character must be a letter
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c
    isAllowedChar c = isAsciiLetter c || isDigit c || c == '-'

-- * Smart Constructors

-- | Creates a generic HTML attribute from a key and value.
attr :: Text -> Text -> Attr
attr = Attr

-- | Creates an id attribute.
id_ :: Text -> Attr
id_ = attr "id"

-- | Creates a class attribute from a list of class names.
class_ :: [Text] -> Attr
class_ = attr "class" . T.unwords

-- | Wraps 'Text' into a 'RawText' 'HTMLBody'.
rawText :: Text -> HTMLBody
rawText = RawText

-- | Wraps a 'String' into a 'RawText' 'HTMLBody'.
rawText' :: String -> HTMLBody
rawText' = fromString

-- | Internal: Helper for formatting text.
textFormat :: Format -> [Attr] -> Text -> HTMLBody
textFormat fmt attrs txt = TextFormat fmt attrs [RawText txt]

-- | Smart constructors for formatting text.
bold, emphasis, subscript, superscript, span :: [Attr] -> Text -> HTMLBody
bold = textFormat Bold
emphasis = textFormat Emphasis
subscript = textFormat Subscript
superscript = textFormat Superscript
span = textFormat Span

-- | Smart constructors for formatting HTML elements.
bold_, emphasis_, subscript_, superscript_ :: [HTMLBody] -> HTMLBody
bold_ = TextFormat Bold []
emphasis_ = TextFormat Emphasis []
subscript_ = TextFormat Subscript []
superscript_ = TextFormat Superscript []

-- | Smart constructors for 'span' elements.
span_ :: [Attr] -> [HTMLBody] -> HTMLBody
span_ = TextFormat Span

-- | Creates a figure containing an image and a caption. The provided attributes
-- are applied to the Figure
figureImage :: [Attr] -> [Attr] -> File -> Text -> Text -> HTMLBody
figureImage attrsFig attrsImg src altText captionTxt =
  Figure attrsFig [Img src altText attrsImg, FigCaption [] [RawText captionTxt]]

-- | Creates an inline script. Does not allow any attributes.
inlineScript :: Text -> HTMLHead
inlineScript = Script []

-- | Creates an external script. Requires a source file/URL and allows optional
-- attributes.
externalScript :: File -> [Attr] -> HTMLHead
externalScript src attrs = Script (attr "src" src : attr "type" "text/javascript" : attrs) mempty

-- | Create the link to the CSS file
stylesheet :: Text -> HTMLHead
stylesheet css = Link "stylesheet" (css <> ".css") [attr "type" "text/css"]
