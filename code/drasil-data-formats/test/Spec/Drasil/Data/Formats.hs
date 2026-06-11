module Spec.Drasil.Data.Formats (formatsTest) where

import Spec.Drasil.Data.Formats.CSV (csvTests)

import Test.Tasty (TestTree, testGroup)

formatsTest :: TestTree
formatsTest =
  testGroup
    "Drasil.Data.Formats"
    [csvTests]
