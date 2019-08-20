module Main (main) where

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Template.Body (srs, printSetting)

main :: IO()
main = do
  gen (DocSpec SRS     "Template_SRS") srs printSetting
  gen (DocSpec Website "Template_SRS") srs printSetting
