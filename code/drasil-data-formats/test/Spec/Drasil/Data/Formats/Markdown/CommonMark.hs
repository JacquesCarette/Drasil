{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.Markdown.CommonMark (commonMarkTests) where

import Data.Text (Text)
import Data.Text qualified as T
import Drasil.Data.Formats.HTML qualified as HTML
import Drasil.Data.Formats.Markdown.CommonMark
  ( Block (..), CodeFenceStyle (..), CommonMark (..), CommonMarkRenderOptions (..),
    EmphasisDelimiter (..), HeadingLevel, HLevel (..), Inline (..), ListItem (..),
    defaultCommonMarkRO, mkHeading, renderCommonMark )
import Drasil.TestingKit.Golden (file, goldenTest, goldenTestingGroup, ps)
import Prettyprinter (LayoutOptions (..), PageWidth (..), defaultLayoutOptions, group, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Spec.Drasil.Data.Formats.Markdown.CommonMark.Inline (inlineTests)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- These tests use the public CommonMark API.
commonMarkTests :: TestTree
commonMarkTests = testGroup "Drasil.Data.Formats.Markdown.CommonMark"
  [ inlineTests,
    codeSyntaxTests,
    headingValidationTests,
    goldenTestingGroup
      [osp|test/build/markdown/commonmark|]
      [osp|test/golden/markdown/commonmark|]
      "Documents"
      [ goldenTest "blocks" $ file [ps|blocks.md|] $
          renderCommonMark defaultCommonMarkRO blocksDocument,
        goldenTest "containers" $ file [ps|containers.md|] $
          renderCommonMark defaultCommonMarkRO containersDocument,
        goldenTest "html" $ file [ps|html.md|] $
          renderCommonMark defaultCommonMarkRO htmlDocument,
        goldenTest "escaping" $ file [ps|escaping.md|] $
          renderCommonMark defaultCommonMarkRO escapingDocument
      ],
    testGroup "Block boundaries"
      [ testCase name (renderText (CommonMark blocks) @?= expected)
      | (name, blocks, expected) <- boundaryCases ],
    testGroup "Heading levels"
      [ testCase (show level) $
          renderText (CommonMark [heading level [Plain "h"]]) @?= marker <> " h"
      | (level, marker) <- [(H1, "#"), (H2, "##"), (H3, "###"),
                           (H4, "####"), (H5, "#####"), (H6, "######")] ],
    testGroup "Independent delimiter options"
      [ testCase (show emphasis <> " / " <> show strong) $
          renderWith (defaultCommonMarkRO
            {emphasisDelimiter = emphasis, strongDelimiter = strong})
            (CommonMark [Paragraph [Emphasis [Plain "small"], Plain " and ", Strong [Plain "large"]]])
            @?= emMarker <> "small" <> emMarker <> " and " <> strongMarker <> "large" <> strongMarker
      | (emphasis, emMarker) <- [(Asterisk, "*"), (Underscore, "_")],
        (strong, strongMarker) <- [(Asterisk, "**"), (Underscore, "__")] ],
    testCase "ordered-list continuation follows a two-digit marker" $
      renderText (CommonMark [OrderedList
        (replicate 9 (ListItem [p "item"]) <> [ListItem [p "ten", p "continued"]])])
        @?= T.intercalate "\n\n" [T.pack (show n) <> ". item" | n <- [1 :: Int .. 9]]
          <> "\n\n10. ten\n\n    continued",
    testCase "group cannot flatten structural newlines" $
      renderStrict (layoutPretty defaultLayoutOptions
        (group (renderCommonMark defaultCommonMarkRO containersDocument)))
        @?= renderText containersDocument,
    testCase "narrow layouts preserve structural newlines" $
      renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 10 1.0))
        (renderCommonMark defaultCommonMarkRO containersDocument))
        @?= renderText containersDocument,
    testCase "group and narrow layout preserve code lines" $
      renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 1 1))
        (group (renderCommonMark defaultCommonMarkRO
          (CommonMark [CodeBlock Nothing "first\n\nlast\n\n"]))))
        @?= "```\nfirst\n\nlast\n\n```",
    testCase "public HTML options reach document fragments" $
      renderWith (defaultCommonMarkRO
        {htmlRenderOptions = HTML.defaultHTMLRO {HTML.indentationSize = 4}})
        (CommonMark [HTMLBlock [HTML.Div [] [HTML.Paragraph [] [HTML.RawText "x"]]]])
        @?= "<div>\n    <p>\n        x\n    </p>\n</div>",
    testGroup "Literal whitespace"
      [ testCase "paragraph edge spaces use character references" $
          renderText (CommonMark [p " x "]) @?= "&#32;x&#32;",
        testCase "four leading spaces stay in a paragraph" $
          renderText (CommonMark [p "    x"]) @?= "&#32;   x",
        testCase "interior spaces and tabs are preserved" $
          renderText (CommonMark [p "a  b\tc"]) @?= "a  b\tc",
        testCase "spaces around soft breaks do not become hard breaks" $
          renderText (CommonMark [Paragraph [Plain "a  ", SoftBreak, Plain "  b"]])
            @?= "a&#32;&#32;\n&#32; b",
        testCase "spaces and tabs retain otherwise-empty paragraphs" $
          renderText (CommonMark [p "  ", p "\t"]) @?= "&#32;&#32;\n\n&#9;",
        testCase "heading edge spaces are trimmed" $
          renderText (CommonMark [heading H1 [Plain "  x  "]]) @?= "# x",
        testCase "heading edge tabs and whitespace-only contents are trimmed" $
          renderText (CommonMark [heading H1 [Plain "\t x \t"],
            heading H2 [Plain " \t "]]) @?= "# x\n\n##",
        testCase "heading trimming preserves spaces between inline nodes" $
          renderText (CommonMark [heading H1 [Plain " A ", Emphasis [Plain "B"], Plain " C "]])
            @?= "# A *B* C",
        testCase "heading trimming preserves code span padding" $
          renderText (CommonMark [heading H1 [Plain " ", CodeSpan " x ", Plain " "]])
            @?= "# `  x  `",
        testCase "heading trimming preserves nonbreaking spaces" $
          renderText (CommonMark [heading H1 [Plain "\x00A0x\x00A0"]])
            @?= "# \x00A0x\x00A0"
      ]
  ]

headingValidationTests :: TestTree
headingValidationTests = testGroup "Heading validation"
  [ testGroup "Reject line breaks before rendering"
      [ testCase name $
          mkHeading H1 contents @?= Left "Headings require inline content without line breaks."
      | (name, contents) <- invalidHeadingCases ],
    testCase "accept nested single-line formatting" $
      fmap (renderText . CommonMark . pure)
        (mkHeading H2 [Strong [Plain "Title"], Plain " ", Link "/target" [CodeSpan "x"]])
        @?= Right "## **Title** [`x`](</target>)",
    testCase "accept inline HTML with renderer layout newlines" $
      fmap (renderText . CommonMark . pure)
        (mkHeading H1 [HTMLInline (HTML.Custom (HTML.CT "span") [] [HTML.RawText "x"])])
        @?= Right "# <span><!-- --> &#32; x </span>"
  ]

invalidHeadingCases :: [(String, [Inline])]
invalidHeadingCases =
  [ ("LF in text", [Plain "a\nb"]),
    ("CR in text", [Plain "a\rb"]),
    ("CRLF in text", [Plain "a\r\nb"]),
    ("CRLF split across nodes", [Plain "a\r", Plain "\nb"]),
    ("soft break", [Plain "a", SoftBreak, Plain "b"]),
    ("hard break", [Plain "a", HardBreak, Plain "b"]),
    ("leading soft break", [SoftBreak, Plain "a"]),
    ("trailing hard break", [Plain "a", HardBreak]),
    ("nested emphasis", [Emphasis [Strong [Plain "a\nb"]]]),
    ("code span newline", [CodeSpan "a\nb"]),
    ("link label", [Link "/target" [Emphasis [HardBreak]]]),
    ("link destination", [Link "/a\nb" [Plain "link"]]),
    ("image description", [Image "image.png" [SoftBreak]]),
    ("image source", [Image "a\rb.png" [Plain "image"]]),
    ("HTML text", [HTMLInline (HTML.RawText "a\nb")]),
    ("nested HTML text", [HTMLInline (HTML.TextFormat HTML.Emphasis [] [HTML.RawText "a\rb"])]),
    ("HTML anchor", [HTMLInline (HTML.Anchor "/a\nb" [] [HTML.RawText "link"])]),
    ("HTML image description", [HTMLInline (HTML.Img "image.png" "a\nb" [])]),
    ("HTML attribute", [HTMLInline (HTML.TextFormat HTML.Span [HTML.Attr "title" "a\nb"] [])]),
    ("HTML comment", [HTMLInline (HTML.Comment "a\nb")]),
    ("custom HTML contents", [HTMLInline (HTML.Custom (HTML.CT "span") [] [HTML.RawText "a\nb"])]),
    ("HTML break element", [HTMLInline (HTML.Custom (HTML.CT "br") [] [])]),
    ("uppercase HTML break element", [HTMLInline (HTML.Custom (HTML.CT "BR") [] [])]),
    ("block HTML", [HTMLInline (HTML.Div [] [HTML.RawText "x"])])
  ]

heading :: HeadingLevel -> [Inline] -> Block
heading level = either error id . mkHeading level

codeSyntaxTests :: TestTree
codeSyntaxTests = testGroup "Code syntax"
  [ testGroup "Code spans"
      [ testCase name $
          renderText (CommonMark [Paragraph [CodeSpan contents]]) @?= expected
      | (name, contents, expected) <- codeCases ],
    testGroup "Code fences"
      [ testCase name $
          renderText (CommonMark [CodeBlock info contents]) @?= expected
      | (name, info, contents, expected) <- fenceCases ],
    testGroup "Code fence style options"
      [ testCase name $
          renderWith (defaultCommonMarkRO {codeFenceStyle = style})
            (CommonMark [CodeBlock info contents]) @?= expected
      | (name, style, info, contents, expected) <- fenceStyleCases ],
    testCase "code languages are independent within one document" $
      renderText (CommonMark
        [CodeBlock (Just "python") "x = 1", CodeBlock (Just "haskell") "x = 1",
         CodeBlock Nothing "plain"])
        @?= "```python\nx = 1\n```\n\n```haskell\nx = 1\n```\n\n```\nplain\n```",
    testCase "fence style does not change inline code" $
      renderWith (defaultCommonMarkRO {codeFenceStyle = TildeFence})
        (CommonMark [Paragraph [CodeSpan "x"]]) @?= "`x`"
  ]

renderText :: CommonMark -> Text
renderText = renderWith defaultCommonMarkRO

renderWith :: CommonMarkRenderOptions -> CommonMark -> Text
renderWith options = renderStrict . layoutPretty defaultLayoutOptions . renderCommonMark options

p :: Text -> Block
p text = Paragraph [Plain text]

oneItem :: Text -> [ListItem]
oneItem text = [ListItem [p text]]

boundaryCases :: [(String, [Block], Text)]
boundaryCases =
  [ ("empty document", [], ""),
    ("empty content is omitted", [Paragraph [], UnorderedList [], OrderedList [], HTMLBlock []], ""),
    ("omitted blocks add no blank lines", [p "a", UnorderedList [], Paragraph [], p "b"], "a\n\nb"),
    ("empty headings, quotes and list items retain markers",
      [heading H1 [], BlockQuote [], UnorderedList [ListItem []]], "#\n\n>\n\n-"),
    ("quote prefixes content and blank lines",
      [BlockQuote [p "a", p "b"]], "> a\n>\n> b"),
    ("empty paragraphs do not add quote lines",
      [BlockQuote [Paragraph []]], ">"),
    ("code normalizes mixed line endings and retains trailing blank lines",
      [CodeBlock Nothing "a\r\nb\rc\n\n"], "```\na\nb\nc\n\n```"),
    ("paragraphs have one separating blank line", [p "a", p "b"], "a\n\nb"),
    ("adjacent unordered lists stay distinct",
      [UnorderedList (oneItem "a"), UnorderedList (oneItem "b")], "- a\n\n<!-- -->\n\n- b"),
    ("adjacent ordered lists stay distinct",
      [OrderedList (oneItem "a"), OrderedList (oneItem "b")], "1. a\n\n<!-- -->\n\n1. b"),
    ("empty blocks do not hide list adjacency",
      [UnorderedList (oneItem "a"), Paragraph [], UnorderedList (oneItem "b")],
      "- a\n\n<!-- -->\n\n- b"),
    ("thematic break remains inside its list item",
      [UnorderedList [ListItem [Paragraph [], ThematicBreak, p "after"]]], "-\n  ---\n\n  after"),
    ("nested empty lists are not a thematic break",
      [UnorderedList [ListItem [UnorderedList [ListItem [UnorderedList [ListItem []]]]]]], "-\n  -\n    -"),
    ("fenced code retains blank lines and ends inside its list item",
      [UnorderedList [ListItem [p "item", CodeBlock Nothing "first\n\nlast"]], p "Outside"],
      "- item\n\n  ```\n  first\n\n  last\n  ```\n\nOutside"),
    ("paragraph boundary breaks keep their intended meaning",
      [Paragraph [SoftBreak, Plain "x", SoftBreak], Paragraph [HardBreak, Plain "x", HardBreak],
       Paragraph [HardBreak]], "x\n\n<br />x<br />\n\n<br /><!-- -->"),
    ("HTML block text is not interpreted as Markdown",
      [HTMLBlock [HTML.RawText "*literal*"]], "<div>\n  *literal*\n</div>"),
    ("HTML blank lines do not expose text as Markdown",
      [HTMLBlock [HTML.Div [] [HTML.RawText "a\n\n*b*"]], Paragraph [Strong [Plain "After"]]],
      "<div>\n  a\n<!-- -->\n  *b*\n</div>\n\n**After**")
  ]

blocksDocument :: CommonMark
blocksDocument = CommonMark
  [ heading H1 [Plain "Document"],
    Paragraph [Plain "Before ", Emphasis [Plain "small"], Plain " and ", Strong [Plain "large"]],
    heading H6 [Plain "Details"],
    CodeBlock (Just "haskell") "before\n```\nafter",
    ThematicBreak,
    Paragraph [Link "https://example.org" [Plain "Link"], Plain " and ", Image "image.png" [Plain "diagram"]]
  ]

containersDocument :: CommonMark
containersDocument = CommonMark
  [ UnorderedList
      [ ListItem
          [ p "parent",
            OrderedList [ListItem [p "first child"], ListItem [p "second child"]],
            p "tail"
          ],
        ListItem [BlockQuote
          [ Paragraph [Plain "quoted first", SoftBreak, Plain "quoted second"],
            p "quoted last"
          ]]
      ],
    p "Outside"
  ]

htmlDocument :: CommonMark
htmlDocument = CommonMark
  [ p "Before",
    HTMLBlock
      [ HTML.Div [HTML.Attr "id" "main-section"] [HTML.Paragraph [] [HTML.RawText "A & B"]],
        HTML.Table [HTML.Attr "id" "table-id"]
          [ HTML.Row [] [HTML.THeader [] [HTML.RawText "Name"], HTML.THeader [] [HTML.RawText "Value"]],
            HTML.Row [] [HTML.TData [] [HTML.RawText "alpha"], HTML.TData [] [HTML.RawText "1 < 2"]]
          ],
        HTML.Heading HTML.H2 [HTML.Attr "id" "main-title"] [HTML.RawText "Anchored heading"],
        HTML.Img "source.png" "Alternative Text" [HTML.Attr "id" "figure-image"]
      ],
    Paragraph [Strong [Plain "After"], Plain " ", Link "#main-title" [Plain "heading"]]
  ]

-- Check that plain text escapes Markdown punctuation.
escapingDocument :: CommonMark
escapingDocument = CommonMark
  [p "These characters should be escaped: *, _, [, ], \\, and #."]

codeCases :: [(String, Text, Text)]
codeCases =
  [ ("empty", "", "<code></code>"),
    ("no Markdown escaping", "a*b", "`a*b`"),
    ("embedded backtick", "a`b", "``a`b``"),
    ("backtick edges", "`x`", "`` `x` ``"),
    ("multiple backticks", "a``b", "```a``b```"),
    ("only backticks", "``", "``` `` ```"),
    ("leading space", " x", "` x`"),
    ("trailing space", "x ", "`x `"),
    ("both spaces", " x ", "`  x  `"),
    ("only spaces", "   ", "`   `"),
    ("normalized newlines", "a\r\nb\rc\nd", "`a b c d`")
  ]

fenceCases :: [(String, Maybe Text, Text, Text)]
fenceCases =
  [ ("ordinary code", Just "haskell", "x = 1", "```haskell\nx = 1\n```"),
    ("empty code", Nothing, "", "```\n```"),
    ("empty info", Just "", "x", "```\nx\n```"),
    ("embedded fence", Nothing, "before\n```\nafter", "````\nbefore\n```\nafter\n````"),
    ("existing final newline", Nothing, "x\n", "```\nx\n```"),
    ("trailing blank lines", Nothing, "x\n\n", "```\nx\n\n```"),
    ("line endings", Nothing, "a\r\nb\rc", "```\na\nb\nc\n```"),
    ("backtick info uses tildes", Just "a`b", "~~~", "~~~~a`b\n~~~\n~~~~"),
    ("info cannot extend the fence", Just "~`x", "hello", "~~~ ~`x\nhello\n~~~"),
    ("info stays on one line", Just "  hs\r\n extra  ", "x", "```hs extra\nx\n```"),
    ("info preserves literal backslashes", Just "a\\b", "x", "```a\\\\b\nx\n```"),
    ("info preserves literal entities", Just "a&copy;", "x", "```a&amp;copy;\nx\n```")
  ]

fenceStyleCases :: [(String, CodeFenceStyle, Maybe Text, Text, Text)]
fenceStyleCases =
  [ ("tilde fence with a language", TildeFence, Just "haskell", "x = 1",
      "~~~haskell\nx = 1\n~~~"),
    ("tilde fence exceeds embedded tildes", TildeFence, Nothing, "before\n~~~\nafter",
      "~~~~\nbefore\n~~~\nafter\n~~~~"),
    ("tilde fence ignores embedded backticks", TildeFence, Nothing, "````",
      "~~~\n````\n~~~"),
    ("backtick preference retains info fallback", BacktickFence, Just "a`b", "~~~",
      "~~~~a`b\n~~~\n~~~~"),
    ("tilde info cannot extend the fence", TildeFence, Just "~hs", "x",
      "~~~ ~hs\nx\n~~~"),
    ("empty tilde code", TildeFence, Nothing, "", "~~~\n~~~"),
    ("tilde fence preserves normalized blank lines", TildeFence, Nothing, "a\r\nb\rc\n\n",
      "~~~\na\nb\nc\n\n~~~")
  ]
