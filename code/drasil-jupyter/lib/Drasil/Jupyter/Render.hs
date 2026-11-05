{-# LANGUAGE OverloadedStrings #-}

module Drasil.Jupyter.Render
  ( CellSourceRenderStyle(..), NotebookRenderOptions(..), defaultNotebookRenderOptions
  , notebookToJSON, renderNotebook
  ) where

import Data.Text (Text)
import qualified Data.Text as T (lines)
import Prettyprinter (Doc)
import Drasil.Data.Formats.JSON
  ( JSON(..), JSONRenderOptions, renderJSON )
import Drasil.Jupyter.Core
  ( Notebook(..), Cell(..), NotebookMetadata(..), KernelSpec(..), LanguageInfo(..) )

-- | JSON representation of cell source.
data CellSourceRenderStyle
  = ListOfLines -- ^ One string per line, with a trailing LF.
  | SingleString -- ^ One string containing the whole cell.
  deriving (Eq, Show)

-- | Notebook format options, separate from JSON layout options.
-- Version numbers label the output; they do not convert the notebook structure.
data NotebookRenderOptions = NotebookRenderOptions
  { notebookFormat :: Int
  , notebookFormatMinor :: Int
  , sourceFormat :: CellSourceRenderStyle
  } deriving (Eq, Show)

-- | Use notebook format 4.4 and a single string per cell by default.
defaultNotebookRenderOptions :: NotebookRenderOptions
defaultNotebookRenderOptions = NotebookRenderOptions 4 4 SingleString

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
cellToJSON :: CellSourceRenderStyle -> Cell -> JSON
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

-- | Line lists append LF to each line; single strings preserve the source verbatim.
formatSource :: CellSourceRenderStyle -> Text -> JSON
formatSource format source = case format of
  ListOfLines -> JArray $ JString <$> sourceLines
  SingleString -> JString source
  where
    sourceLines = (<> "\n") <$> T.lines source
