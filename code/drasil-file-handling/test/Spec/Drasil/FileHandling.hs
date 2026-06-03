module Spec.Drasil.FileHandling (artifactsTests) where

import Spec.Drasil.FileHandling.FilePath (filePathTests)
import Spec.Drasil.FileHandling.FileLayout (fileLayoutTests)
import Test.Tasty (TestTree, testGroup)

artifactsTests :: TestTree
artifactsTests = testGroup
    "Drasil.FileHandling"
    [ filePathTests
    , fileLayoutTests
    ]
