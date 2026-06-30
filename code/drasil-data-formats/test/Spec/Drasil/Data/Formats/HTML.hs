{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.HTML (htmlTests) where

import Drasil.Data.Formats.HTML (
  HTML(..), HTMLBody(..), HTMLHead(..), Format(..), HLevel(..), Row(..), Cell(..),
  LItem(..), DItem(..), ListType(..), Attribute(..), renderHTML
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
    [ Link   [Attr "href" "style.css"],
      Title  "Test File", Meta [Attr "charset" "utf-8"],
      Script [] "/* The script should be here */",
      Script [Attr "src" "source/script.hs", BoolAttr "async"] ""
    ]
    [ Div [Attr "id" "main-section"]
      [ Heading H1 [Attr "class" "title"] [RawText "tagsHTMLTest"],
        Heading H2 [Attr "class" "h2"] [RawText "tagsHTMLTest"],
        Heading H3 [Attr "class" "h3"] [RawText "tagsHTMLTest"],

        Paragraph [Attr "class" "paragraph"]
          [ RawText "Testing paragraph and text formats: ",
            TextFormat Bold        [Attr "id" "bold"]        [RawText "bold, "],
            TextFormat Emphasis    [Attr "id" "emphasis"]    [RawText "emphasis, "],
            TextFormat Subscript   [Attr "id" "subscript"]   [RawText "subscript, "],
            TextFormat Superscript [Attr "id" "superscript"] [RawText "superscript, "],
            TextFormat Span        [Attr "id" "span"]        [RawText "span."]
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
         [Anchor "https://jacquescarette.github.io/Drasil/" [Attr "id" "ancor"] [RawText "Anchor"]],

       Figure [Attr "class" "figure"]
         [ Img "source.png" [Attr "alt" "Alternative Text"],
           FigCaption [Attr "class" "figcaption"] [RawText "Figure Caption"]
         ],

       CustomTag "blockquote" [Attr "class" "quote"]
         [Paragraph [] [RawText "This is a quote."]],

       EmptyCustomTag "input" [Attr "class" "input"]
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
