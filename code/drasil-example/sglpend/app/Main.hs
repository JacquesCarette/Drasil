module Main (main) where

import Drasil.Generator (caseStudyMainSRS)

import Drasil.SglPend.Body (mkSRS, si)

main :: IO ()
main = caseStudyMainSRS si mkSRS "SglPend_SRS"
