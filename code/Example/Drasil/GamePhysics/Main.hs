module Main where

import Language.Drasil (DocType(SRS, Website), Recipe(..), gen)

import Drasil.GamePhysics.Body (chipmunkSRS', everything)

docs :: [Recipe]
docs = [Recipe (SRS "Chipmunk_SRS") chipmunkSRS',
        Recipe (Website "Chipmunk_SRS") chipmunkSRS'
       ]

main :: IO ()
main = do
  gen docs everything
