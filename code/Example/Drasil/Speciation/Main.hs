module Main (main) where

import Language.Drasil

import Drasil.Speciation.Body (specn_srs)
import Drasil.Speciation.Symbols (specnSymbMap)

docs :: [Recipe]
docs = 
  [Recipe (SRS "Speciation_SRS")     specn_srs, 
   Recipe (Website "Speciation_SRS") specn_srs
  ]

main :: IO()
main = do
  gen docs specnSymbMap
