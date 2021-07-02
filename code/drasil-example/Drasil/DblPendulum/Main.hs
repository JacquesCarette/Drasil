module Drasil.DblPendulum.Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(SRS, Website))
import Drasil.DblPendulum.Body (srs, printSetting, si)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS     "DblPendulum_SRS") srs printSetting
  gen (DocSpec Website "DblPendulum_SRS") srs printSetting
  genDot si
