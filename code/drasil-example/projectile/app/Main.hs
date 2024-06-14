  
module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genDot, 
  DocSpec(DocSpec), DocType(SRS,Jupyter), Format(..), docChoices,
  dumpEverything)
import Drasil.Projectile.Body (printSetting, srs, fullSI)
import Drasil.Projectile.Choices (choiceCombos, genCodeWithChoices)

import qualified Drasil.Projectile.Lesson.Body as PL (nb, printSetting)


main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "Projectile_SRS") srs printSetting
  gen (DocSpec (docChoices Jupyter [])      "Projectile Lesson") PL.nb PL.printSetting
  genCodeWithChoices choiceCombos
  genDot fullSI
  -- if the chunkDB had a mutable state, then this would make more sense.
  -- dumpChunkDB fullSI ".drasil/" "fruits.json"
  
  