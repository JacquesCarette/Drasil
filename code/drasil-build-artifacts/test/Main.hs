module Main (main) where

import Test.Tasty
import Spec.Drasil.Build.Artifacts (artifactsTests)

main :: IO ()
main = defaultMain artifactsTests
