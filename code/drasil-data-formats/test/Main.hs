module Main (main) where

import Drasil.TestingKit.TestMain (testMain)
import Spec.Drasil.Data.Formats (formatsTest)

main :: IO ()
main = testMain formatsTest
