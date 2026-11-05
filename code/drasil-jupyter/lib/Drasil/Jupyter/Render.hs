{-# LANGUAGE OverloadedStrings #-}

module Drasil.Jupyter.Render
  ( SourceFormat(..), NotebookRenderOptions(..), defaultNotebookRenderOptions
  , notebookToJSON, renderNotebook
  ) where

import Data.Text (Text)
import qualified Data.Text as T (concat, lines)
import Prettyprinter (Doc)
import Drasil.Data.Formats.JSON
  ( JSON(..), JSONRenderOptions, renderJSON )
import Drasil.Jupyter.Core
  ( Notebook(..), Cell(..), NotebookMetadata(..), KernelSpec(..), LanguageInfo(..) )

-- | JSON representation of cell source. Both forms append LF to each
-- LF-separated line, including an unterminated last line; empty source stays empty.
data SourceFormat
  = SourceLines -- ^ One string per line, for readable diffs.
  | SourceString -- ^ One string containing the whole cell.
  deriving (Eq, Show)

-- | Notebook format options, separate from JSON layout options.
-- Version numbers label the output; they do not convert the notebook structure.
data NotebookRenderOptions = NotebookRenderOptions
  { notebookFormat :: Int
  , notebookFormatMinor :: Int
  , sourceFormat :: SourceFormat
  } deriving (Eq, Show)

-- | Preserve the format version and source layout used by the current generators.
defaultNotebookRenderOptions :: NotebookRenderOptions
defaultNotebookRenderOptions = NotebookRenderOptions 4 4 SourceLines

-- | Encode a new notebook using the selected version and source representation.
notebookToJSON :: NotebookRenderOptions -> Notebook -> JSON
notebookToJSON options (Notebook metadata cells) = JObject
  [ ("cells", JArray $ cellToJSON (sourceFormat options) <$> cells)
  , ("metadata", metadataToJSON metadata)
  , ("nbformat", JNumber $ fromIntegral $ notebookFormat options)
  , ("nbformat_minor", JNumber $ fromIntegral $ notebookFormatMinor options)
  ]

-- | Render a notebook with the shared JSON renderer.
renderNotebook :: NotebookRenderOptions -> JSONRenderOptions -> Notebook -> Doc ann
renderNotebook options jsonOptions = renderJSON jsonOptions . notebookToJSON options

metadataToJSON :: NotebookMetadata -> JSON
metadataToJSON metadata = JObject
  [ ("kernelspec", kernelToJSON $ notebookKernel metadata)
  , ("language_info", languageToJSON $ notebookLanguage metadata)
  ]

kernelToJSON :: KernelSpec -> JSON
kernelToJSON kernel = JObject
  [ ("display_name", JString $ kernelDisplayName kernel)
  , ("language", JString $ kernelLanguage kernel)
  , ("name", JString $ kernelName kernel)
  ]

languageToJSON :: LanguageInfo -> JSON
languageToJSON language = JObject
  [ ("codemirror_mode", languageCodeMirrorMode language)
  , ("file_extension", JString $ languageFileExtension language)
  , ("mimetype", JString $ languageMimeType language)
  , ("name", JString $ languageName language)
  , ("nbconvert_exporter", JString $ languageExporter language)
  , ("pygments_lexer", JString $ languageLexer language)
  , ("version", JString $ languageVersion language)
  ]

-- | Encode a new cell; code is unexecuted and has no outputs.
cellToJSON :: SourceFormat -> Cell -> JSON
cellToJSON format (MarkdownCell metadata source) = JObject
  [ ("cell_type", "markdown")
  , ("metadata", JObject metadata)
  , ("source", formatSource format source)
  ]
cellToJSON format (CodeCell metadata source) = JObject
  [ ("cell_type", "code")
  , ("execution_count", JNull)
  , ("metadata", JObject metadata)
  , ("outputs", JArray [])
  , ("source", formatSource format source)
  ]

-- | Both representations preserve the existing source convention: split at LF
-- and append LF to each resulting line, including an unterminated last line.
formatSource :: SourceFormat -> Text -> JSON
formatSource format source = case format of
  SourceLines -> JArray $ JString <$> sourceLines
  SourceString -> JString $ T.concat sourceLines
  where
    sourceLines = (<> "\n") <$> T.lines source
