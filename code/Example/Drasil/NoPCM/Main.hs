module Main (main) where

import Language.Drasil (DocType(SRS,Website),Recipe(..),gen, genCode)

import Drasil.NoPCM.Body (nopcm_srs, nopcm_code)

docs :: [Recipe]
docs = [Recipe (SRS "NoPCM_SRS") nopcm_srs,
        Recipe (Website "NoPCM_SRS") nopcm_srs
       ]

main :: IO ()            
main = do
  gen docs
  genCode nopcm_code
