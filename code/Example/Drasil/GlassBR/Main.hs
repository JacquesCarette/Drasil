module Main (main) where

import Language.Drasil

import Drasil.GlassBR.Body (glassBR_srs, glassBR_code, glassChoices)
import Drasil.GlassBR.Symbols (gbSymbMap)

docs :: [Recipe]
docs = 
  [Recipe (SRS "GlassBR_SRS")     glassBR_srs, 
   Recipe (Website "GlassBR_SRS") glassBR_srs
  ]

main :: IO()
main = do
  gen docs gbSymbMap
  genCode glassChoices glassBR_code
