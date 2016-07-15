module Main where

import Language.Drasil (DocType(SRS, Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.GamePhysics.ChipmunkBody (chip_srs)

docs :: [Recipe]
docs = [Recipe (SRS "Chipmunk_SRS") chip_srs,
        Recipe (Website "Chipmunk_SRS") chip_srs
       ]

main :: IO ()
main = do
  gen docs
