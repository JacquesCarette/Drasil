module Main where

import Language.Drasil (DocType(SRS,LPM,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

--import Example.Drasil.HGHC (srsBody,lpmBody)
import Example.Drasil.GamePhysics.ChipBody (chip_srs)

docs :: [Recipe]
docs = [{- Recipe (SRS "SRS")     srsBody,
        Recipe (Website "SRS") srsBody,
--        Recipe SRS "PCM_SRS.tex" createSRS2,
        Recipe (LPM "LPM")     lpmBody, -}
        Recipe (SRS "Chipmunk_SRS") chip_srs,
        Recipe (Website "Chipmunk_SRS") chip_srs
       ]

main :: IO ()
main = do
  gen docs
