module Main where

import Language.Drasil (DocType(SRS,Website),Recipe(..),gen)

import Drasil.NoPCM.Body (pcm_srs)

docs :: [Recipe]
docs = [Recipe (SRS "NoPCM_SRS") pcm_srs,
        Recipe (Website "NoPCM_SRS") pcm_srs
       ]

main :: IO ()            
main = do
  gen docs
