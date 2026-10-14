module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCode)

import Drasil.DblPend.Body (mkSRS, si)
import Drasil.DblPend.Choices (choices)

main :: IO ()
main = caseStudyMainSRSWCode si mkSRS "DblPend_SRS" choices
