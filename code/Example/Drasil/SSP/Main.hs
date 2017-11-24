module Main (main) where

import Language.Drasil (DocType(SRS,Website), Recipe(Recipe), gen)

import Drasil.SSP.Body (ssp_srs, sspSymMap)

docs :: [Recipe]
docs = [Recipe (Website "SSP_SRS") ssp_srs,
        Recipe (SRS "SSP_SRS") ssp_srs
       ]

main :: IO ()            
main = do
  gen docs sspSymMap
  --genCode ssp_code
