{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.Jupyter.Render
  ( CellSourceRenderStyle(..), NotebookRenderOptions(..), defaultNotebookRenderOptions
  , notebookToJSON, renderNotebook
  ) where

import Data.Text (Text)
import qualified Data.Text as T (lines, pack)
import Prettyprinter (Doc)
import Drasil.Data.Formats.JSON
  ( JSON(..), JSONRenderOptions, renderJSON )
import Drasil.Data.Formats.Jupyter.Core
  ( Notebook(..), Cell(..), NotebookMetadata(..), KernelSpec(..), LanguageInfo(..) )

-- | JSON representation of cell source.
data CellSourceRenderStyle
  = ListOfLines -- ^ One string per line, with a trailing LF.
  | SingleString -- ^ One string containing the whole cell.
  deriving (Eq, Show)

-- | Notebook format options, separate from JSON layout options.
-- Format 4.5 and later minor versions include cell IDs.
data NotebookRenderOptions = NotebookRenderOptions
  { notebookFormat :: Int
  , notebookFormatMinor :: Int
  , sourceFormat :: CellSourceRenderStyle
  } deriving (Eq, Show)

-- | Use notebook format 4.5 and a list of lines per cell by default.
defaultNotebookRenderOptions :: NotebookRenderOptions
defaultNotebookRenderOptions = NotebookRenderOptions 4 5 ListOfLines

-- | Encode a new notebook using the selected version and source representation.
notebookToJSON :: NotebookRenderOptions -> Notebook -> JSON
notebookToJSON options (Notebook metadata cells) = JObject
  [ ("cells", JArray $ zipWith (cellToJSON options) [0..] cells)
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
-- IDs use cell positions in this newly generated notebook.
cellToJSON :: NotebookRenderOptions -> Int -> Cell -> JSON
cellToJSON options index cell = JObject $ cellId ++ case cell of
  MarkdownCell metadata source ->
    [ ("cell_type", "markdown")
    , ("metadata", JObject metadata)
    , ("source", formatSource format source)
    ]
  CodeCell metadata source ->
    [ ("cell_type", "code")
    , ("execution_count", JNull)
    , ("metadata", JObject metadata)
    , ("outputs", JArray [])
    , ("source", formatSource format source)
    ]
  where
    format = sourceFormat options
    cellId = [("id", JString $ "cell-" <> T.pack (show index))
      | notebookFormat options == 4 && notebookFormatMinor options >= 5]

-- | Line lists append LF to each line; single strings preserve the source verbatim.
formatSource :: CellSourceRenderStyle -> Text -> JSON
formatSource format source = case format of
  ListOfLines -> JArray $ JString <$> sourceLines
  SingleString -> JString source
  where
    sourceLines = (<> "\n") <$> T.lines source
