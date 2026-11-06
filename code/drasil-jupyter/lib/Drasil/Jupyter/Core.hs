{-# LANGUAGE OverloadedStrings #-}

module Drasil.Jupyter.Core
  ( Notebook(..), Cell(..), Metadata
  , NotebookMetadata(..), KernelSpec(..), LanguageInfo(..)
  , defaultPython39Metadata
  ) where

import Data.Text (Text)
import Drasil.Data.Formats.JSON (JSON(..))

-- | Cell metadata stored as JSON object entries.
type Metadata = [(Text, JSON)]

-- | A new notebook containing ordered cells and document metadata.
data Notebook = Notebook
  NotebookMetadata -- ^ Notebook-level metadata.
  [Cell] -- ^ Cells in document order.
  deriving (Eq, Show)

-- | Notebook kernel and language information.
data NotebookMetadata = NotebookMetadata
  { notebookKernel :: KernelSpec
  , notebookLanguage :: LanguageInfo
  } deriving (Eq, Show)

-- | The kernel used to execute a notebook.
data KernelSpec = KernelSpec
  { kernelDisplayName :: Text
  , kernelLanguage :: Text
  , kernelName :: Text
  } deriving (Eq, Show)

-- | Notebook language information.
data LanguageInfo = LanguageInfo
  { languageName :: Text
  , languageCodeMirrorMode :: JSON
  , languageFileExtension :: Text
  , languageMimeType :: Text
  , languageExporter :: Text
  , languageLexer :: Text
  , languageVersion :: Text
  } deriving (Eq, Show)

-- | Python 3.9.1 metadata used by the current notebook generators.
defaultPython39Metadata :: NotebookMetadata
defaultPython39Metadata = NotebookMetadata
  { notebookKernel = KernelSpec "Python 3" "python" "python3"
  , notebookLanguage = LanguageInfo
      { languageName = "python"
      , languageCodeMirrorMode =
          JObject [("name", "ipython"), ("version", JNumber 3)]
      , languageFileExtension = ".py"
      , languageMimeType = "text/x-python"
      , languageExporter = "python"
      , languageLexer = "ipython3"
      , languageVersion = "3.9.1"
      }
  }

-- | A Markdown cell or an unexecuted code cell.
data Cell
  = MarkdownCell
      Metadata -- ^ Cell-level metadata.
      -- TODO: Use the CommonMark AST from #5406 for Markdown cell content.
      Text -- ^ Rendered Markdown source.
  | CodeCell
      Metadata -- ^ Cell-level metadata.
      Text -- ^ Code source.
  deriving (Eq, Show)
