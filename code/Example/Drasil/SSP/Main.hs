module Main (main) where

import Language.Drasil (DocType(SRS,MG,Website), Recipe(Recipe), gen, genCode)

import Drasil.SSP.Body (ssp_srs, ssp_mg, ssp_code, sspSymMap)

docs :: [Recipe]
docs = [Recipe (Website "SSP_SRS") ssp_srs,
        Recipe (SRS "SSP_SRS") ssp_srs,
        --Recipe (Website "SSP_MG") ssp_mg,
        Recipe (MG "SSP_MG") ssp_mg
       ]

main :: IO ()            
main = do
  gen docs sspSymMap
  --genCode ssp_code
