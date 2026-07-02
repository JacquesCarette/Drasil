module Main (main) where

import Drasil.Generator (caseStudyMainLsnPlan)

import Drasil.Projectile.Lesson.Body (si, nbDecl)

main :: IO ()
main = caseStudyMainLsnPlan si nbDecl "Projectile_Lesson"
