module Spec.Drasil.Build.Artifacts (artifactsTests) where

import Spec.Drasil.Build.Artifacts.FilePath (filePathTests)
import Spec.Drasil.Build.Artifacts.FileLayout (fileLayoutTests)
import Test.Tasty (TestTree, testGroup)

artifactsTests :: TestTree
artifactsTests = testGroup
    "Drasil.Build.Artifacts"
    [ filePathTests
    , fileLayoutTests
    ]
