module Drasil.Jupyter.Core
  ( Notebook(..), Cell(..), Metadata
  ) where

import Data.Text (Text)
import Drasil.Data.Formats.JSON (JSON)

-- | Notebook or cell metadata stored as JSON object entries.
type Metadata = [(Text, JSON)]

-- | A new notebook containing ordered cells and document metadata.
data Notebook = Notebook
  Metadata -- ^ Notebook-level metadata.
  [Cell] -- ^ Cells in document order.
  deriving (Eq, Show)

-- | A Markdown cell or an unexecuted code cell.
data Cell
  = MarkdownCell
      Metadata -- ^ Cell-level metadata.
      Text -- ^ Rendered Markdown source.
  | CodeCell
      Metadata -- ^ Cell-level metadata.
      Text -- ^ Code source.
  deriving (Eq, Show)
