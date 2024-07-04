module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genDot,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything,
  MDFlavour(GitHub))
import Drasil.GamePhysics.Body (srs, printSetting, fullSI)

main :: IO ()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON, Markdown GitHub, MDBook]) "GamePhysics_SRS") srs  printSetting
  genDot fullSI
  -- When ready to generate code from GamePhysics, uncomment the next line and all of Choices.hs
  -- genCode choices code
