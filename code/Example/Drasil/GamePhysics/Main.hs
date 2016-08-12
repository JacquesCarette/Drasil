module Main where

import Language.Drasil (DocType(SRS, MG, Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.GamePhysics.ChipmunkBody (chipmunkSRS, chipmunkMG)

docs :: [Recipe]
docs = [Recipe (SRS "Chipmunk_SRS") chipmunkSRS,
        Recipe (Website "Chipmunk_SRS") chipmunkSRS,
        -- Recipe (Website "Chipmunk_MG") chipmunkMG,
        Recipe (MG "Chipmunk_MG") chipmunkMG
       ]

main :: IO ()
main = do
  gen docs
