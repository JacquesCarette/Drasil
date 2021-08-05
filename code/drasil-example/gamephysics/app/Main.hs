module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.GamePhysics.Body (srs, printSetting, fullSI)


main :: IO ()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX]) "GamePhysics_SRS") srs  printSetting
  genDot fullSI
  -- When ready to generate code from GamePhysics, uncomment the next line and all of Choices.hs
  -- genCode choices code
