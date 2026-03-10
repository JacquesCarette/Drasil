module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrs)

import Drasil.BinaryStar.Body (mkSRS, si)

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrs si mkSRS "BSS_SRS"
