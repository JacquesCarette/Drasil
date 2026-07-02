module Spec.Drasil.Data.Formats (formatsTest) where

import Spec.Drasil.Data.Formats.CSV (csvTests)
import Spec.Drasil.Data.Formats.JSON (jsonTests)

import Test.Tasty (TestTree, testGroup)

formatsTest :: TestTree
formatsTest =
  testGroup
    "Drasil.Data.Formats"
    [ csvTests,
      jsonTests
    ]
