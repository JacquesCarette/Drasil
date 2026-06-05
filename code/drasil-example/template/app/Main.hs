module Main (main) where

import Drasil.Generator (caseStudyMainSRS)

import Drasil.Template.Body (mkSRS, si)

main :: IO ()
main = caseStudyMainSRS si mkSRS "Template_SRS"
