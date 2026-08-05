module Drasil.Data.Formats.MD.Core
  ( -- * Markdown
    Markdown(..), URL, Source, ListType(..), ListItem
  )
where

import Data.Text (Text)

data Markdown =
    Heading Int (Maybe Text) [Markdown]
  | Div Text [Markdown]
  | Code (Maybe Text) Text
  | Quote [Markdown]
  | Link URL [Markdown]
  | Image Source [Markdown] (Maybe Text)
  | List ListType [ListItem]
  | Table [[Markdown]] [[Markdown]] (Maybe [Markdown]) (Maybe Text)
  | Paragraph [Markdown]
  | Bold [Markdown]
  | Italic [Markdown]
  | RawText Text
  | Line
  deriving (Eq, Show)

-- | Target of the Link
type URL = Text

-- | Source for the image
type Source = Text

-- | Type of list
data ListType = Ordered | Unordered
  deriving (Eq, Show)

-- | List item
type ListItem = [Markdown]
