module Main (main) where

import Test.Tasty (defaultMain)
import Spec.Drasil.FileHandling (artifactsTests)

main :: IO ()
main = defaultMain artifactsTests
