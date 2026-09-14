module Drasil.Data.Formats.MD.Core
  ( -- * Markdown
    Markdown(..), URL, Source, ListType(..), ListItem
  )
where

import Data.Text (Text)

data Markdown =
    Heading Int (Maybe Text) [Markdown] -- ^ Heading level, optional ID and content
  | Div Text [Markdown] -- ^ ID and content
  | Code (Maybe Text) Text -- ^ Optional language and code content
  | Quote [Markdown]
  | Link URL [Markdown] -- ^ URL and content
  | Image Source [Markdown] (Maybe Text) -- ^ Source, content and optional ID
  | List ListType [ListItem]
  | Table [[Markdown]] [[Markdown]] (Maybe [Markdown]) (Maybe Text) -- ^ Header rows, data rows, optional caption and optional ID
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
