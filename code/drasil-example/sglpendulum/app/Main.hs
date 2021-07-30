module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS), Format(..))
import Drasil.SglPendulum.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (SRS [HTML, TeX]) "SglPendulum_SRS") srs printSetting
  genDot fullSI
