{-# LANGUAGE OverloadedStrings #-}

module Drasil.Jupyter.Render
  ( notebookToJSON, renderNotebook
  ) where

import Data.Text (Text)
import qualified Data.Text as T (lines)
import Prettyprinter (Doc)
import Drasil.Data.Formats.JSON
  ( JSON(..), JSONRenderOptions, renderJSON )
import Drasil.Jupyter.Core (Notebook(..), Cell(..))

-- | Encode a new notebook using the existing notebook format version.
notebookToJSON :: Notebook -> JSON
notebookToJSON (Notebook metadata cells) = JObject
  [ ("cells", JArray $ cellToJSON <$> cells)
  , ("metadata", JObject metadata)
  , ("nbformat", JNumber 4)
  , ("nbformat_minor", JNumber 4)
  ]

-- | Render a notebook with the shared JSON renderer.
renderNotebook :: JSONRenderOptions -> Notebook -> Doc ann
renderNotebook options = renderJSON options . notebookToJSON

-- | Encode a new cell; code is unexecuted and has no outputs.
cellToJSON :: Cell -> JSON
cellToJSON (MarkdownCell metadata source) = JObject
  [ ("cell_type", "markdown")
  , ("metadata", JObject metadata)
  , ("source", formatSource source)
  ]
cellToJSON (CodeCell metadata source) = JObject
  [ ("cell_type", "code")
  , ("execution_count", JNull)
  , ("metadata", JObject metadata)
  , ("outputs", JArray [])
  , ("source", formatSource source)
  ]

-- | Preserve the existing generated-notebook source convention: split at LF
-- and append LF to each resulting line, including an unterminated last line.
formatSource :: Text -> JSON
formatSource source = JArray $ JString . (<> "\n") <$> T.lines source
