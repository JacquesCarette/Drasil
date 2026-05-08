module Spec.Drasil.Build.Artifacts (artifactsTests) where

import Spec.Drasil.Build.Artifacts.FilePath (filePathTests)
import Test.Tasty

artifactsTests :: TestTree
artifactsTests =
  testGroup
    "Drasil.Build.Artifacts"
    [filePathTests]
