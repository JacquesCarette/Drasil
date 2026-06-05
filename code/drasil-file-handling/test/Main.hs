module Main (main) where

import Drasil.TestingKit.TestMain (testMain)
import Spec.Drasil.FileHandling (artifactsTests)

main :: IO ()
main = testMain artifactsTests
