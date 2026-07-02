module Main (main) where

import Drasil.TestingKit (testMain)
import Spec.Drasil.Assets (assetTests)

main :: IO ()
main = testMain assetTests
