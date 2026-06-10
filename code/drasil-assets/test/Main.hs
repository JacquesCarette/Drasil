module Main (main) where

import Test.Tasty (defaultMain)
import Spec.Drasil.Assets (assetTests)

main :: IO ()
main = defaultMain assetTests
