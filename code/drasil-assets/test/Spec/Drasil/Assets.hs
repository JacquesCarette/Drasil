{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Drasil.Assets (assetTests) where

import Drasil.Assets
import Drasil.Build.Artifacts
import System.OsPath
import Test.Tasty (TestTree, testGroup)

assetTests :: TestTree
assetTests =
  testGroup
    "Drasil.Assets"
    [ readAssetTests
    ]

readAssetTests :: TestTree
readAssetTests =
  testGroup
    "readAsset"
    [ goldenTestingGroup
        [osp|test/build|]
        [osp|test/golden|]
        "Golden Tests"
        [ goldenTest "Single file" helloWorldFile
        ]
    ]

helloWorldFile :: FileLayout
helloWorldFile =
  let asset = $$(readAsset [osp|test/golden/hello-world.txt|] "hello world test file")
   in asset `toFile` [ps|hello-world.txt|]
