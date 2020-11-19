module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.DblPendulum.Body (srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS     "DblPendulum_SRS") srs printSetting
  gen (DocSpec Website "DblPendulum_SRS") srs printSetting
