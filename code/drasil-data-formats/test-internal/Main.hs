{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Drasil.Data.Formats.Markdown.CommonMark.Render
  ( RenderedLines, addToFirstLine, addToLastLine, appendRenderedLines,
    concatenateRenderedLines, docToRenderedLines, emptyInlineLines,
    normalizeLineEndings, prefixBlockQuote, renderCodeFence, renderCodeSpan,
    renderedLinesToDoc, separateRenderedBlocks, singletonLine,
    textToRenderedLines, wrapRenderedLines )
import Prettyprinter
  ( LayoutOptions (..), PageWidth (..), defaultLayoutOptions, group,
    hardline, layoutPretty )
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "CommonMark foundations and code syntax"
  [ testGroup "physical lines"
      [ testCase name (output actual @?= expected)
      | (name, actual, expected) <- lineCases ],
    testCase "normalize mixed line endings" $
      normalizeLineEndings "a\r\nb\rc\n\n" @?= "a\nb\nc\n\n",
    testCase "group and narrow layout preserve physical newlines" $
      renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 1 1))
        (group (renderedLinesToDoc (textToRenderedLines "first\n\nlast"))))
        @?= "first\n\nlast",
    testGroup "literal code spans"
      [ testCase name (renderCodeSpan contents @?= expected)
      | (name, contents, expected) <- codeCases ],
    testGroup "code fences"
      [ testCase name (output (renderCodeFence info contents) @?= expected)
      | (name, info, contents, expected) <- fenceCases ]
  ]

output :: RenderedLines () -> Text
output = renderStrict . layoutPretty defaultLayoutOptions . renderedLinesToDoc

lineCases :: [(String, RenderedLines (), Text)]
lineCases =
  [ ("text preserves trailing blank lines", linesOf "a\r\nb\r\n\r\n", "a\nb\n\n"),
    ("empty inline content", emptyInlineLines, ""),
    ("single physical line", singletonLine "one", "one"),
    ("append joins only touching lines",
      appendRenderedLines (linesOf "a\nb") (linesOf "c\nd"), "a\nbc\nd"),
    ("append preserves blank boundaries",
      appendRenderedLines (linesOf "a\n") (linesOf "\nb"), "a\n\nb"),
    ("append absent left", appendRenderedLines absent (linesOf "a\nb"), "a\nb"),
    ("append absent right", appendRenderedLines (linesOf "a\nb") absent, "a\nb"),
    ("append two absent results", appendRenderedLines absent absent, ""),
    ("wrap first and last lines", wrapRenderedLines "*" (linesOf "a\n\nb"), "*a\n\nb*"),
    ("wrap absent result", wrapRenderedLines "*" absent, "**"),
    ("prefix creates a line", addToFirstLine "start" absent, "start"),
    ("suffix creates a line", addToLastLine "end" absent, "end"),
    ("quote prefixes each line", prefixBlockQuote (linesOf "a\n\nb\n"), "> a\n>\n> b\n>"),
    ("quote absent result", prefixBlockQuote absent, ">"),
    ("quote empty inline result", prefixBlockQuote emptyInlineLines, ">"),
    ("prefix makes blank line nonempty",
      prefixBlockQuote (addToFirstLine "x" emptyInlineLines), "> x"),
    ("suffix makes blank line nonempty",
      prefixBlockQuote (addToLastLine "x" emptyInlineLines), "> x"),
    ("blocks use one blank separator",
      separateRenderedBlocks [linesOf "a", linesOf "b"], "a\n\nb"),
    ("absent blocks add no separator",
      separateRenderedBlocks [absent, linesOf "a", absent, linesOf "b", absent], "a\n\nb"),
    ("all absent blocks", separateRenderedBlocks [absent, absent], ""),
    ("concatenate inserts no separator",
      concatenateRenderedLines [linesOf "a", absent, linesOf "b"], "a\nb"),
    ("document conversion keeps physical lines",
      docToRenderedLines ("a" <> hardline <> hardline <> "b"), "a\n\nb")
  ]
  where
    linesOf = textToRenderedLines
    absent = concatenateRenderedLines []

codeCases :: [(String, Text, Text)]
codeCases =
  [ ("empty", "", ""),
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
