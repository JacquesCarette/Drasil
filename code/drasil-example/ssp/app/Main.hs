module Main (main) where

import Drasil.Generator (caseStudyMainSRS)

import Drasil.SSP.Body (mkSRS, si)

main :: IO ()
main = caseStudyMainSRS si mkSRS "SSP_SRS"
