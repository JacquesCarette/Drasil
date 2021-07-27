module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, DocType(SRS, Website), DocSpec(DocSpec))
import Drasil.GamePhysics.Body (srs, printSetting, fullSI)


main :: IO ()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS "GamePhysics_SRS") srs  printSetting
  gen (DocSpec Website "GamePhysics_SRS") srs printSetting
  genDot fullSI
  -- When ready to generate code from GamePhysics, uncomment the next line and all of Choices.hs
  -- genCode choices code
