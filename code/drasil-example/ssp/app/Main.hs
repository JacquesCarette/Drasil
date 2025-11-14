module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrs)

import Drasil.SSP.Body (mkSRS, si)

main :: IO ()
main = do
  setLocaleEncoding utf8
  _ <- exportSmithEtAlSrs si mkSRS "SSP_SRS"
  return ()
