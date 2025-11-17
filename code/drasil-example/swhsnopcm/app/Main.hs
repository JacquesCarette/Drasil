module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrsWCode)

import Drasil.SWHSNoPCM.Body (mkSRS, si)
import Drasil.SWHSNoPCM.Choices (choices)

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrsWCode si mkSRS "SWHSNoPCM_SRS" choices []
