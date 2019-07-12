module Main (main) where

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Projectile.Body (srs, printSetting)

main :: IO()
main = do
  gen (DocSpec SRS     "Projectile_SRS") srs printSetting
  gen (DocSpec Website "Projectile_SRS") srs printSetting
