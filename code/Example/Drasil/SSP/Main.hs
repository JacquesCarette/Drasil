module Main where

import Language.Drasil (DocType(SRS,MG,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.SSP.SSPBody (ssp_srs, ssp_mg)

docs :: [Recipe]
docs = [Recipe (Website "SSP_SRS") ssp_srs,
        Recipe (SRS "SSP_SRS") ssp_srs,
        --Recipe (Website "SSP_MG") ssp_mg,
        Recipe (MG "SSP_MG") ssp_mg
       ]

main :: IO ()            
main = do
  gen docs
