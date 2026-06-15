{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.JSON (jsonTests) where

import Drasil.Data.Formats.JSON (JSON(..), jsonRenderOpts, renderJSON,
  JSONRenderOptions, JSONStyle(..))

import Drasil.FileHandling (file, ps)
import Drasil.TestingKit.Golden (goldenTestingGroup, goldenTest)
import Data.Scientific (scientific)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)

jsonTests :: TestTree
jsonTests =
  testGroup
    "Drasil.Data.Formats.JSON"
    [ renderJSONTests
    ]

valueTestJSON :: JSON
valueTestJSON =
  JObject
  [ ("true", JBool True),
    ("false", JBool False),
    ("null", JNull),
    ("number", JNumber 4.5),
    ("sciNumber", JNumber (scientific 1234 (-6))),
    ("string", "hello"),
    ("snowman", "☃"),
    ("array",
      JArray
      [ JNumber 1,
        JNumber 2,
        JNumber 3,
        JNumber (-1),
        JNumber (scientific 1234 10),
        JNumber (scientific (-1234) (-10))
      ]
    ),
    ("emptyArray", JArray []),
    ("emptyObject", JObject [])
  ]

escapingTestJSON :: JSON
escapingTestJSON =
  JObject
  [ ("a", JString "abc\"\\\b\f\n\r\t\x0000\x001F"),
    ("abc\"\\\b\f\n\r\t\x0000\x001F", JString "b")
  ]

ugly :: JSONRenderOptions
ugly = jsonRenderOpts Minified

pretty :: JSONRenderOptions
pretty = jsonRenderOpts (Pretty 2)

renderJSONTests :: TestTree
renderJSONTests =
  testGroup
    "renderJSON"
    [ goldenTestingGroup
      [osp|test/build/json|]
      [osp|test/golden/json|]
      "Golden Tests"
      [ goldenTest "valueTestJSON ugly" $
          file [ps|values_ugly.json|] $ renderJSON ugly valueTestJSON,
        goldenTest "valueTestJSON pretty" $
          file [ps|values_pretty.json|] $ renderJSON pretty valueTestJSON,
        goldenTest "escapingTestJSON ugly" $
          file [ps|escaping_ugly.json|] $ renderJSON ugly escapingTestJSON,
        goldenTest "escapingTestJSON pretty" $
          file [ps|escaping_pretty.json|] $ renderJSON pretty escapingTestJSON
      ]
    ]
