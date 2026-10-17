{-# LANGUAGE OverloadedStrings #-}

-- Focused regressions through the public document-rendering API. Ordinary
-- inline constructors are already exercised by the complete-document goldens.
module Spec.Drasil.Data.Formats.Markdown.CommonMark.Inline (inlineTests) where

import Data.Text (Text)
import Drasil.Data.Formats.HTML qualified as HTML
import Drasil.Data.Formats.Markdown.CommonMark
  ( Block (Paragraph), CommonMark (..), Inline (..),
    defaultCommonMarkRO, renderCommonMark )
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

inlineTests :: TestTree
inlineTests = testGroup "Inline regressions through the public API"
  [ testCase name (renderParagraph nodes @?= expected)
  | (name, nodes, expected) <- inlineCases ]

renderParagraph :: [Inline] -> Text
renderParagraph nodes = renderStrict $ layoutPretty defaultLayoutOptions $
  renderCommonMark defaultCommonMarkRO (CommonMark [Paragraph nodes])

-- Expected Markdown is handwritten. Character references protect literal
-- whitespace; HTML fallbacks keep neighboring delimiters from merging.
inlineCases :: [(String, [Inline], Text)]
inlineCases =
  [ -- A CRLF pair may span text nodes, even with absent nodes between them.
    ("split CRLF and empty nodes do not create a blank line",
      [Plain "a\r", CodeSpan "", HTMLInline (HTML.RawText ""), Plain "\nb"],
      "a\nb"),
    -- Nesting and touching emphasis must retain their separate boundaries.
    ("nested and adjacent emphasis remain separate",
      [Emphasis [Strong [Plain "inner"]], Plain " ",
        Emphasis [Plain "a"], Plain "", CodeSpan "", Emphasis [Plain "b"]],
      "<em>**inner**</em> <em>a</em><em>b</em>"),
    ("adjacent code spans preserve literal HTML and Markdown",
      [CodeSpan "<&", HTMLInline (HTML.RawText ""), CodeSpan "*x*"],
      "<code>&lt;&amp;</code><code>\\*x\\*</code>"),
    -- A tag-only opening line would otherwise start a CommonMark HTML block.
    ("TAB then LF in emphasis stays inline",
      [Emphasis [Plain "\t\nx"]],
      "<em><!-- -->&#9;\nx</em>"),
    -- CommonMark normally trims indentation following a source newline.
    ("literal indentation inside emphasis survives a newline",
      [Emphasis [Plain "a\n\tb"]],
      "*a\n&#9;b*"),
    ("literal indentation survives across Plain and HTML nodes",
      [Plain "a\n", HTMLInline (HTML.RawText "\tb")],
      "a\n&#9;b"),
    ("spaces before an HTML literal newline do not make a hard break",
      [Plain "a  ", HTMLInline (HTML.RawText "\nb")],
      "a&#32;&#32;\nb"),
    -- Custom inline elements need the same recursive text protection as em.
    ("Custom HTML preserves literal Markdown and its indentation",
      [Plain "before ",
        HTMLInline (HTML.Custom (HTML.CT "span") [] [HTML.RawText "*literal*"]),
        Plain " after"],
      "before <span><!-- -->\n&#32; \\*literal\\*\n</span> after"),
    ("HTML text is escaped once while Markdown remains literal",
      [HTMLInline (HTML.TextFormat HTML.Emphasis [] [HTML.RawText "<&*x*>"])],
      "<em>&lt;&amp;\\*x\\*&gt;</em>"),
    -- Destination encoding is separate from label escaping, and keeps %20.
    ("link destination and label use their own escaping rules",
      [Link "/a b<c>\\d?x=&copy;&ok=%20" [Plain "read[x]"]],
      "[read\\[x\\]](</a%20b%3Cc%3E%5Cd?x=&amp;copy;&amp;ok=%20>)"),
    ("relative link preserves query parameters and fragment",
      [Link "target.md?mode=one&other=two#target-heading" [Plain "query"]],
      "[query](<target.md?mode=one&amp;other=two#target-heading>)"),
    ("Markdown suffixes in query and fragment stay literal",
      [Link "target.md?next=other.md#section.md" [Plain "next"]],
      "[next](<target.md?next=other.md#section.md>)"),
    ("encoded query delimiters stay encoded",
      [Link "target.md?value=a%26b%23c#target-heading" [Plain "encoded"]],
      "[encoded](<target.md?value=a%26b%23c#target-heading>)"),
    ("image description supports formatting and code syntax",
      [Image "my photo.png" [Strong [Plain "plot"], Plain " ", CodeSpan "x"]],
      "![**plot** `x`](<my%20photo.png>)"),
    ("interior soft and hard breaks stay distinct",
      [Plain "a", SoftBreak, Plain "b", HardBreak, Plain "c"],
      "a\nb\\\nc")
  ]
