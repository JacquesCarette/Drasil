{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.HTML (htmlTests) where

import Drasil.Data.Formats.HTML (
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), HLevel(..), CustomTag(..),
    Row(..), Cell(..), LItem(..), DItem(..), ListType(..), Attr(..), renderHTML, renderHTMLFragment, defaultHTMLRO,
    bold, emphasis, subscript, superscript, figureImage, customTag,
    HTMLRenderOptions(..)
  )

import qualified Drasil.Data.Formats.HTML as HTML (span)
import Drasil.TestingKit.Golden (file, goldenTest, goldenTestingGroup, ps)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text (Text)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import qualified Data.Map as M

htmlTests :: TestTree
htmlTests =
  testGroup
    "Drasil.Data.Formats.HTML"
    [ renderHTMLTests,
      renderHTMLFragmentTests
    ]

blockquoteTag, inputTag :: CustomTag
blockquoteTag = customTag "blockquote"
inputTag      = customTag "input"

testRenderOptions :: HTMLRenderOptions
testRenderOptions = HTMLRO (M.fromList [
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
          file [ps|tags.html|] $ renderHTML testRenderOptions tagsHTMLTest,

        goldenTest "escapingHTMLTest" $
          file [ps|escaping.html|] $ renderHTML testRenderOptions  escapingHTMLTest
      ]
    ]

-- | Body-only output must retain the existing HTML rendering rules.
renderHTMLFragmentTests :: TestTree
renderHTMLFragmentTests =
  testGroup "renderHTMLFragment"
    [ testCase "empty fragment" $
        fragment [] @?= "",
      testCase "paragraph without document wrappers" $
        fragment [Paragraph [] [RawText "A & B < C"]]
          @?= "<p>\n  A &amp; B &lt; C\n</p>",
      testCase "escape raw text exactly once" $
        fragment [RawText "< > & \" '"] @?= "&lt; &gt; &amp; &quot; &#39;",
      testCase "merge adjacent text without an extra newline" $
        fragment [RawText "Hello", RawText " world"] @?= "Hello world",
      testCase "preserve block order and line boundaries" $
        fragment [Paragraph [] [RawText "First"], Paragraph [] [RawText "Second"]]
          @?= "<p>\n  First\n</p>\n<p>\n  Second\n</p>",
      testCase "honour indentation in nested fragments" $
        fragmentWith (defaultHTMLRO {indentationSize = 4})
          [Div [] [Paragraph [] [RawText "Hello"]]]
          @?= "<div>\n    <p>\n        Hello\n    </p>\n</div>",
      testCase "inline HTML remains inline" $
        fragment [emphasis [] "Hello"] @?= "<em>Hello</em>"
    ]
  where
    fragment = fragmentWith defaultHTMLRO
    fragmentWith :: HTMLRenderOptions -> [HTMLBody] -> Text
    fragmentWith opt = renderStrict . layoutPretty defaultLayoutOptions . renderHTMLFragment opt
