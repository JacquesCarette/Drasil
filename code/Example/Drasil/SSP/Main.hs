module Main where

import Language.Drasil (DocType(SRS,LPM,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.SSP.SSPBody (ssp_srs)

docs :: [Recipe]
docs = [Recipe (Website "SSP_SRS") ssp_srs,
        Recipe (SRS "SSP_SRS") ssp_srs
       ]

main :: IO ()            
main = do
  gen docs
