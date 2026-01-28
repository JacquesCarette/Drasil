module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrsWCode)

import Drasil.DblPend.Body (mkSRS, si)
import Drasil.DblPend.Choices (choices)

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrsWCode si mkSRS "DblPend_SRS" choices
