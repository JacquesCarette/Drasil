module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCode)

import Drasil.SWHSNoPCM.Body (mkSRS, si)
import Drasil.SWHSNoPCM.Choices (choices)

main :: IO ()
main = caseStudyMainSRSWCode si mkSRS "SWHSNoPCM_SRS" choices
