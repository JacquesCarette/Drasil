module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genDot, genLog,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)
import Drasil.GamePhysics.Body (srs, printSetting, fullSI)

main :: IO ()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "GamePhysics_SRS") srs  printSetting
  genDot fullSI
  genLog fullSI printSetting
  -- When ready to generate code from GamePhysics, uncomment the next line and all of Choices.hs
  -- genCode choices code
