module Main where

import Language.Drasil (DocType(SRS, MG, Website), Recipe(..), gen)

import Drasil.GamePhysics.Body (chipmunkSRS', chipmunkMG, cpSymbMap)

docs :: [Recipe]
docs = [Recipe (SRS "Chipmunk_SRS") chipmunkSRS',
        Recipe (Website "Chipmunk_SRS") chipmunkSRS',
        -- Recipe (Website "Chipmunk_MG") chipmunkMG,
        Recipe (MG "Chipmunk_MG") chipmunkMG
       ]

main :: IO ()
main = do
  gen docs cpSymbMap
