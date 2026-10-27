{-# LANGUAGE OverloadedStrings #-}

-- | Construct Jupyter notebook cells from rendered text, independently of the
-- pretty-printing library used to produce it. JSON escaping is left to the
-- JSON renderer.
module Language.Drasil.JSON.Cell (
  markdownCellFromText, codeCellFromText
) where

import Data.Text (Text)
import qualified Data.Text as T (lines)

import Drasil.Data.Formats.JSON (JSON(..))

-- | Construct a Markdown cell from rendered Markdown text.
markdownCellFromText :: Text -> JSON
markdownCellFromText contents =
  JObject
  [ ("cell_type", "markdown"),
    ("metadata", JObject []),
    ("source", formatSource contents)
  ]

-- | Construct an unexecuted code cell from source text, with no outputs.
codeCellFromText :: Text -> JSON
codeCellFromText contents =
  JObject
  [ ("cell_type", "code"),
    ("execution_count", JNull),
    ("metadata", JObject []),
    ("outputs", JArray []),
    ("source", formatSource contents)
  ]

-- | Preserve the existing notebook source convention: split at LF and append
-- LF to every resulting line, including an unterminated final line. Empty
-- input produces an empty array; blank lines and carriage returns are retained.
formatSource :: Text -> JSON
formatSource contents = JArray $ JString . (<> "\n") <$> T.lines contents
