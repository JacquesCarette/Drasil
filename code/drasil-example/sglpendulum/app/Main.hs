module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS, Website))
import Drasil.SglPendulum.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS     "SglPendulum_SRS") srs printSetting
  gen (DocSpec Website "SglPendulum_SRS") srs printSetting
  genDot fullSI
