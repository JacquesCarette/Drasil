module Main (main) where

import Drasil.TestingKit (testMain)
import Spec.Language.Drasil.JSON.Cell (cellTests)

main :: IO ()
main = testMain cellTests
