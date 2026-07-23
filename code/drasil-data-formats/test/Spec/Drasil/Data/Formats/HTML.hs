{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.HTML (htmlTests) where

import Drasil.Data.Formats.HTML (
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), HLevel(..), CustomTag(..),
    Row(..), Cell(..), LItem(..), DItem(..), ListType(..), Attr(..), renderHTML,
    bold, emphasis, subscript, superscript, figureImage, customTag,
    HTMLRenderOptions(..)
  )

import qualified Drasil.Data.Formats.HTML as HTML (span)
import Drasil.TestingKit.Golden (file, goldenTest, goldenTestingGroup, ps)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)
import qualified Data.Map as M

htmlTests :: TestTree
htmlTests =
  testGroup
    "Drasil.Data.Formats.HTML"
    [ renderHTMLTests
    ]

blockquoteTag, inputTag :: CustomTag
blockquoteTag = customTag "blockquote"
inputTag      = customTag "input"

testGenOptions :: HTMLRenderOptions
testGenOptions = HTMLBO (M.fromList [
    (blockquoteTag, Standard),
    (inputTag, Void)
  ]) 2

tagsHTMLTest :: HTML
tagsHTMLTest =
  HTML
    [ Link   "stylesheet" "style.css" [],
      Title  "Test File", Meta [Attr "charset" "utf-8"],
      Script [] "/* The script should be here */",
      Script [Attr "src" "source/script.hs", Attr "async" ""] ""
    ]
    [ Div [Attr "id" "main-section"]
      [ Heading H1 [Attr "class" "title"] [RawText "tagsHTMLTest"],
        Heading H2 [Attr "class" "h2"] [RawText "tagsHTMLTest"],
        Heading H3 [Attr "class" "h3"] [RawText "tagsHTMLTest"],

        Paragraph [Attr "class" "paragraph"]
          [ RawText "Testing paragraph and text formats: ",
            bold        [Attr "id" "bold"]        "bold, ",
            emphasis    [Attr "id" "emphasis"]    "emphasis, ",
            subscript   [Attr "id" "subscript"]   "subscript, ",
            superscript [Attr "id" "superscript"] "superscript, ",
            HTML.span   [Attr "id" "span"]        "span."
          ],

        List Ordered [Attr "id" "ordered-list"]
          [ LItem [] [RawText "Item 1"],
            LItem [] [RawText "Item 2"],
            LItem [] [RawText "Item 3"]
          ],

        List Unordered [Attr "id" "unordered-list"]
          [ LItem [] [RawText "Item 1"],
            LItem [] [RawText "Item 2"],
            LItem [] [RawText "Item 3"]
          ],

       Table [Attr "class" "table"]
         [ Row [Attr "class" "row"]
           [ THeader [Attr "class" "table-header"] [RawText "Header1"],
             TData [Attr "class" "data-cell"]      [RawText "Data cell 1"]
           ],
           Row [Attr "class" "row"]
           [ THeader [Attr "class" "table-header"] [RawText "Header2"],
             TData [Attr "class" "data-cell"]      [RawText "Data cell 2"]]
         ],

       DescriptionList [Attr "id" "dlist"]
         [ DTerm [Attr "id" "dterm"]       [RawText "Description Term"],
           DDetails [Attr "id" "ddetails"] [RawText "Description Details"]
         ],

       Paragraph []
         [Anchor "https://jacquescarette.github.io/Drasil/" [Attr "id" "anchor"] [RawText "Anchor"]],

       figureImage [Attr "id" "figure-image"] [] "source.png" "Alternative Text" "Figure Caption",

       Custom blockquoteTag [Attr "class" "quote"]
         [Paragraph [] [RawText "This is a quote."]],

       Custom inputTag [Attr "class" "input"] []
      ]
    ]

escapingHTMLTest :: HTML
escapingHTMLTest =
  HTML
    [ Title "Escaping Characters" ]
    [ Paragraph []
        [ RawText "These characters should be escaped: <, >, &, \", and '." ]]

renderHTMLTests :: TestTree
renderHTMLTests =
  testGroup
    "renderHTML"
    [ goldenTestingGroup
      [osp|test/build/html|]
      [osp|test/golden/html|]
      "Golden Tests"
      [ goldenTest "tagsHTMLTest" $
          file [ps|tags.html|] $ renderHTML testGenOptions tagsHTMLTest,

        goldenTest "escapingHTMLTest" $
          file [ps|escaping.html|] $ renderHTML testGenOptions  escapingHTMLTest
      ]
    ]
