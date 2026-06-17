module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCodeZoo)

import Drasil.Projectile.Body (si, mkSRS)
import Drasil.Projectile.Choices (choiceCombos)

main :: IO ()
main = caseStudyMainSRSWCodeZoo si mkSRS "Projectile_SRS" choiceCombos
