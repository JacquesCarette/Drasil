module Main (main) where

import Drasil.Generator (caseStudyMainSRS)

import Drasil.GamePhysics.Body (mkSRS, si)

main :: IO ()
main = caseStudyMainSRS si mkSRS "GamePhysics_SRS"
