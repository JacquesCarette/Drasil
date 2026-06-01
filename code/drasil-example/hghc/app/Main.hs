module Main (main) where

import Drasil.Generator (caseStudyMainSRS)

import Drasil.HGHC.Body (mkSRS, si)

main :: IO ()
main = caseStudyMainSRS si mkSRS "HGHC_SRS"
