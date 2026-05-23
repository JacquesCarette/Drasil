module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCode)

import Drasil.PDController.Body (mkSRS, si)
import Drasil.PDController.Choices (choices)

main :: IO ()
main = caseStudyMainSRSWCode si mkSRS "PDController_SRS" choices
