module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrs)

import Drasil.SglPend.Body (mkSRS, si)

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrs si mkSRS "SglPend_SRS"
