module Main (main) where

import Test.Tasty (defaultMain)
import Spec.Drasil.Build.Artifacts (artifactsTests)

main :: IO ()
main = defaultMain artifactsTests
