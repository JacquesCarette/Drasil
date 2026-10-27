{-# LANGUAGE OverloadedStrings #-}

module Spec.Language.Drasil.JSON.Cell (cellTests) where

import Data.Text (Text)
import Drasil.Data.Formats.JSON (JSON(..), JSONStyle(Minified),
  jsonRenderOpts, renderJSON)
import Language.Drasil.JSON.Cell (markdownCellFromText, codeCellFromText)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

cellTests :: TestTree
cellTests =
  testGroup
    "Language.Drasil.JSON.Cell"
    [ cellShapeTests,
      sourceTests "Markdown source" markdownCellFromText,
      sourceTests "Code source" codeCellFromText,
      contentTests,
      serializationTest
    ]

cellShapeTests :: TestTree
cellShapeTests =
  testGroup
    "Cell structure"
    [ testCase "Markdown cells contain only type, metadata and source" $
        markdownCellFromText "# Title" @?=
          JObject
          [ ("cell_type", "markdown"),
            ("metadata", JObject []),
            ("source", JArray ["# Title\n"])
          ],
      testCase "Code cells start unexecuted with no outputs" $
        codeCellFromText "x = 1" @?=
          JObject
          [ ("cell_type", "code"),
            ("execution_count", JNull),
            ("metadata", JObject []),
            ("outputs", JArray []),
            ("source", JArray ["x = 1\n"])
          ]
    ]

-- These fixed expectations describe the existing notebook newline policy.
-- In particular, a nonempty final line receives a newline even without one
-- in the input; an empty input produces no source lines.
sourceTests :: String -> (Text -> JSON) -> TestTree
sourceTests name makeCell =
  testGroup
    name
    [ testCase "Empty input has no lines" $
        assertSource (makeCell "") [],
      testCase "The final line receives a newline" $
        assertSource (makeCell "alpha") ["alpha\n"],
      testCase "An existing final newline is not duplicated" $
        assertSource (makeCell "alpha\n") ["alpha\n"],
      testCase "A single blank line is retained" $
        assertSource (makeCell "\n") ["\n"],
      testCase "Leading and interior blank lines are retained" $
        assertSource (makeCell "\nalpha\n\nbeta")
          ["\n", "alpha\n", "\n", "beta\n"],
      testCase "A trailing blank line is retained" $
        assertSource (makeCell "alpha\n\n") ["alpha\n", "\n"],
      testCase "Indentation and trailing spaces are retained" $
        assertSource (makeCell "  alpha  \n\tbeta ")
          ["  alpha  \n", "\tbeta \n"],
      testCase "CRLF retains its carriage return" $
        assertSource (makeCell "alpha\r\nbeta") ["alpha\r\n", "beta\n"],
      testCase "A bare carriage return does not split a line" $
        assertSource (makeCell "alpha\rbeta") ["alpha\rbeta\n"]
    ]

contentTests :: TestTree
contentTests =
  testGroup
    "Literal source content"
    [ testCase "Markdown keeps quotes, backslashes, tabs and Unicode" $
        assertSource (markdownCellFromText "say \"hello\"\\path\t雪")
          ["say \"hello\"\\path\t雪\n"],
      testCase "Math text reaches the JSON AST without pre-escaping" $
        assertSource (markdownCellFromText "$x_{ab} + \\frac{1}{2}$")
          ["$x_{ab} + \\frac{1}{2}$\n"],
      testCase "HTML attributes reach the JSON AST without pre-escaping" $
        assertSource (markdownCellFromText "<a id=\"target\">A & B</a>")
          ["<a id=\"target\">A & B</a>\n"],
      testCase "Code keeps indentation, operators and quoted text" $
        assertSource (codeCellFromText "if x < 2:\n    print(\"a\\b\")")
          ["if x < 2:\n", "    print(\"a\\b\")\n"]
    ]

serializationTest :: TestTree
serializationTest =
  testCase "The JSON renderer escapes notebook source exactly once" $
    renderStrict (layoutPretty defaultLayoutOptions rendered) @?= expected
  where
    rendered = renderJSON (jsonRenderOpts Minified) $
      markdownCellFromText "say \"hello\"\\path\t雪"
    expected = "{\"cell_type\":\"markdown\",\"metadata\":{}," <>
      "\"source\":[\"say \\\"hello\\\"\\\\path\\t雪\\n\"]}"

assertSource :: JSON -> [Text] -> Assertion
assertSource (JObject fields) expected =
  lookup "source" fields @?= Just (JArray $ JString <$> expected)
assertSource other _ = assertFailure $ "Expected a cell object, got: " <> show other
