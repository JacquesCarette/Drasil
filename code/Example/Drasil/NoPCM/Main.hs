module Main where

import Language.Drasil (DocType(SRS,Website),Recipe(..),gen)

import Drasil.NoPCM.Body (nopcm_srs)

docs :: [Recipe]
docs = [Recipe (SRS "NoPCM_SRS") nopcm_srs,
        Recipe (Website "NoPCM_SRS") nopcm_srs
       ]

main :: IO ()            
main = do
  gen docs
