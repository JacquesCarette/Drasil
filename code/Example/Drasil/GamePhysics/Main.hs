module Main where

import Language.Drasil (DocType(SRS, Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.GamePhysics.ChipmunkBody (chipmunkSRS)

docs :: [Recipe]
docs = [Recipe (SRS "Chipmunk_SRS") chipmunkSRS,
        Recipe (Website "Chipmunk_SRS") chipmunkSRS
       ]

main :: IO ()
main = do
  gen docs
