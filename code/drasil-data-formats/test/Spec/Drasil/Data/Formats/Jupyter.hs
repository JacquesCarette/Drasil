{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.Jupyter (jupyterTests) where

import Drasil.Data.Formats.JSON (JSON(..), objectField, pretty)
import Drasil.Data.Formats.Jupyter (Cell(..), CellSourceRenderStyle(..),
  KernelSpec(..), LanguageInfo(..), Notebook(..), NotebookMetadata(..),
  NotebookRenderOptions(..), defaultNotebookRenderOptions,
  defaultPython39Metadata, renderJupyter, renderJupyter')

import Drasil.FileHandling (file, ps)
import Drasil.TestingKit.Golden (goldenTestingGroup, goldenTest)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

jupyterTests :: TestTree
jupyterTests =
  testGroup
    "Drasil.Data.Formats.Jupyter"
    [ renderJupyterTests,
      renderJupyter'Tests
    ]

exampleNotebook :: Notebook
exampleNotebook = Notebook defaultPython39Metadata
  [ MarkdownCell [] "# Example",
    CodeCell [] "x = 1\nprint(x)"
  ]

customMetadata :: NotebookMetadata
customMetadata = NotebookMetadata
  { notebookKernel = KernelSpec "Julia 1.10" "julia" "julia-1.10"
  , notebookLanguage = LanguageInfo
      { languageName = "julia"
      , languageCodeMirrorMode = JString "julia"
      , languageFileExtension = ".jl"
      , languageMimeType = "application/julia"
      , languageExporter = "script"
      , languageLexer = "julia"
      , languageVersion = "1.10.0"
      }
  }

renderJupyterTests :: TestTree
renderJupyterTests =
  testGroup
    "renderJupyter"
    [ testCase "custom kernel metadata" $
        (metadataJSON >>= objectField "kernelspec") @?= Just (JObject
          [ ("display_name", "Julia 1.10"),
            ("language", "julia"),
            ("name", "julia-1.10")
          ]),
      testCase "custom language information" $
        (metadataJSON >>= objectField "language_info") @?= Just (JObject
          [ ("codemirror_mode", "julia"),
            ("file_extension", ".jl"),
            ("mimetype", "application/julia"),
            ("name", "julia"),
            ("nbconvert_exporter", "script"),
            ("pygments_lexer", "julia"),
            ("version", "1.10.0")
          ]),
      testCase "Markdown tags and slideshow metadata" $ do
        let metadata =
              [ ("tags", JArray ["overview"]),
                ("slideshow", JObject [("slide_type", "slide")])
              ]
        cellMetadata (MarkdownCell metadata "# Overview")
          @?= Just (JObject metadata),
      testCase "code tags and visibility metadata" $ do
        let metadata =
              [ ("tags", JArray ["parameters"]),
                ("jupyter", JObject [("source_hidden", JBool True)])
              ]
        cellMetadata (CodeCell metadata "iterations = 100")
          @?= Just (JObject metadata),
      testCase "selected notebook format 4.4" $ do
        let options = defaultNotebookRenderOptions { notebookFormatMinor = 4 }
            notebookJSON = renderJupyter options exampleNotebook
        objectField "nbformat_minor" notebookJSON @?= Just (JNumber 4)
        case objectField "cells" notebookJSON of
          Just (JArray cells) -> fmap (objectField "id") cells @?= [Nothing, Nothing]
          _ -> assertFailure "Expected a cells array",
      testCase "cell IDs for later major versions" $ do
        let options = defaultNotebookRenderOptions
              { notebookFormat = 5, notebookFormatMinor = 0 }
        case objectField "cells" (renderJupyter options exampleNotebook) of
          Just (JArray cells) -> fmap (objectField "id") cells
            @?= [Just (JString "cell-0"), Just (JString "cell-1")]
          _ -> assertFailure "Expected a cells array"
    ]
  where
    metadataJSON = objectField "metadata" $
      renderJupyter defaultNotebookRenderOptions $
        Notebook customMetadata [MarkdownCell [] "# Report"]

cellMetadata :: Cell -> Maybe JSON
cellMetadata cell = do
  JArray [cellJSON] <- objectField "cells" $
    renderJupyter defaultNotebookRenderOptions (Notebook defaultPython39Metadata [cell])
  objectField "metadata" cellJSON

renderJupyter'Tests :: TestTree
renderJupyter'Tests =
  testGroup
    "renderJupyter'"
    [ goldenTestingGroup
      [osp|test/build/jupyter|]
      [osp|test/golden/jupyter|]
      "Golden Tests"
      [ goldenTest "ListOfLines" $
          file [ps|list_of_lines.ipynb|] $
            renderJupyter' defaultNotebookRenderOptions
              pretty exampleNotebook,
        goldenTest "SingleString" $
          file [ps|single_string.ipynb|] $
            renderJupyter' (defaultNotebookRenderOptions { sourceFormat = SingleString })
              pretty exampleNotebook
      ]
    ]
