module Main (main) where

import Language.Drasil

import Drasil.GlassBR.Body (glassBR_srs, glassBR_mg, glassBR_code, glassChoices)
import Drasil.GlassBR.Symbols (gbSymbMap)

docs :: [Recipe]
docs = 
  [Recipe (SRS "GlassBR_SRS")     glassBR_srs, 
   Recipe (Website "GlassBR_SRS") glassBR_srs,
   Recipe (MG "GlassBR_MG")       glassBR_mg
  ]

main :: IO()
main = do
  gen docs gbSymbMap
  genCode glassChoices glassBR_code
