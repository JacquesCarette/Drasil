{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
-- ^ HLint reports `TemplateHaskell` is unused, but we use it in
-- `helloWorldFile` below.

module Spec.Drasil.Assets (assetTests) where

import Drasil.Assets
import Drasil.FileHandling
import Drasil.TestingKit.GoldenTesting
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
