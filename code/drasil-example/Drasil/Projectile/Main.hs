module Main (main) where

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Projectile.Body (srsDoc, printSetting)

main :: IO()
main = do
  gen (DocSpec SRS     "Projectile_SRS") srsDoc printSetting
  gen (DocSpec Website "Projectile_SRS") srsDoc printSetting
