module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrsWCode)

import Drasil.BinaryStar.Body (mkSRS, si)
import Drasil.BinaryStar.Choices (choices)

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrsWCode si mkSRS "BSS_SRS" choices
