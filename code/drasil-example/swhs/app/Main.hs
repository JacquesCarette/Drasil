module Main (main) where

import Drasil.Generator (caseStudyMainSRS)

import Drasil.SWHS.Body (mkSRS, si)

main :: IO ()
main = caseStudyMainSRS si mkSRS "SWHS_SRS"
