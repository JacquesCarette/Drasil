{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.MD (mdTests) where

import Drasil.Data.Formats.MD (
    Markdown(..), ListType(..),
    renderMarkdown, MDRenderOptions(..), MDFlavour(..), TableStyle(..)
  )

import Drasil.TestingKit.Golden (file, goldenTest, goldenTestingGroup, ps)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)

mdTests :: TestTree
mdTests = testGroup "Drasil.Data.Formats.MD" [ renderMDTests ]

testRenderOptions :: MDRenderOptions
testRenderOptions = MDRO {
  mdFlavour = Pandoc,
  tableStyle = Pretty,
  formatChar = '*'
}

tagsMDTest :: [Markdown]
tagsMDTest =
  [ Heading 1 (Just "main-title") [RawText "Test Document"],
    Heading 2 Nothing [RawText "Section 1"],
    Div "main-section" [
      Code (Just "Haskell") "This is a code block",
      Quote [RawText "This is a quote."],
      Paragraph
      [ RawText "Testing paragraph with ", Bold [RawText "bold"], RawText " and ",
        Italic [RawText "italic"], RawText " text." ],
      List Ordered [[RawText "First item"], [RawText "Second item"]],
      List Unordered [[RawText "Bullet 1"], [RawText "Bullet 2"]]
    ],
    Table [[RawText "Header 1", RawText "Header 2"]]
      [[RawText "Data 1", RawText "Data 2"], [RawText "Data 3",   RawText "Data 4"]]
      mempty (Just "table-id"),
    Line,
    Paragraph [ RawText "Testing link: ", Link "#main-title" [RawText "link"], RawText "."],
    Image "source.png" [RawText "Alternative Text"] (Just "figure-image")
  ]

escapingMDTest :: [Markdown]
escapingMDTest =
  [ Paragraph
    [ RawText "These characters should be escaped: *, _, [, ], \\, and #." ]
  ]

renderMDTests :: TestTree
renderMDTests = testGroup "renderMarkdown"
  [ goldenTestingGroup
    [osp|test/build/md|]
    [osp|test/golden/md|]
    "Golden Tests"
    [ goldenTest "tagsMDTest" $
      file [ps|tags.md|] $ renderMarkdown testRenderOptions tagsMDTest,

      goldenTest "escapingMDTest" $
      file [ps|escaping.md|] $ renderMarkdown testRenderOptions escapingMDTest
    ]
  ]
