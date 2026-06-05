module Main (main) where

import Drasil.TestingKit.TestMain (testMain)
import Spec.Drasil.Assets (assetTests)

main :: IO ()
main = testMain assetTests
