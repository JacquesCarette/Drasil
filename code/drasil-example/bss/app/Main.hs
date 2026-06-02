module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCode)

import Drasil.BinaryStar.Body (mkSRS, si)
import Drasil.BinaryStar.Choices (choices)

main :: IO ()
main = caseStudyMainSRSWCode si mkSRS "BSS_SRS" choices
