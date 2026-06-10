module Main (main) where

import Test.Tasty (defaultMain)
import Spec.Drasil.Data.Formats (formatsTest)

main :: IO ()
main = defaultMain formatsTest
