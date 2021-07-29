module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS))
import Drasil.DblPendulum.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS     "DblPendulum_SRS") srs printSetting
  genDot fullSI
