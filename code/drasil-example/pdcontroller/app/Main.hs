module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrsWCode)

import Drasil.PDController.Body (mkSRS, si)
import Drasil.PDController.Choices (choices)

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrsWCode si mkSRS "PDController_SRS" choices
