module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCode)

import Drasil.GlassBR.Body (mkSRS, si)
import Drasil.GlassBR.Choices (choices)

main :: IO ()
main = caseStudyMainSRSWCode si mkSRS "GlassBR_SRS" choices
