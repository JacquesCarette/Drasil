{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.HTML (htmlTests) where

import Drasil.Data.Formats.HTML (
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), HLevel(..),
    Row(..), Cell(..), LItem(..), DItem(..), ListType(..), Attr(..), renderHTML,
    boldText, emphasisText, subscriptText, superscriptText, spanText, figureImage
  )

import Drasil.TestingKit.Golden (file, goldenTest, goldenTestingGroup, ps)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)

htmlTests :: TestTree
htmlTests =
  testGroup
    "Drasil.Data.Formats.HTML"
    [ renderHTMLTests
    ]

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
            boldText        [Attr "id" "bold"]        "bold, ",
            emphasisText    [Attr "id" "emphasis"]    "emphasis, ",
            subscriptText   [Attr "id" "subscript"]   "subscript, ",
            superscriptText [Attr "id" "superscript"] "superscript, ",
            spanText        [Attr "id" "span"]        "span."
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

       figureImage "source.png" "Alternative Text" "Figure Caption",

       CustomTag "blockquote" Standard [Attr "class" "quote"]
         [Paragraph [] [RawText "This is a quote."]],

       CustomTag "input" Void [Attr "class" "input"] []
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
          file [ps|tags.html|] $ renderHTML tagsHTMLTest,

        goldenTest "escapingHTMLTest" $
          file [ps|escaping.html|] $ renderHTML escapingHTMLTest
      ]
    ]
