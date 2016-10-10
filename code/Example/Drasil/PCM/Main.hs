module Main where

import Language.Drasil (DocType(SRS,Website),Recipe(..),gen)

import Drasil.PCM.Body (pcm_srs)

docs :: [Recipe]
docs = [Recipe (SRS "PCM_SRS") pcm_srs,
        Recipe (Website "PCM_SRS") pcm_srs
       ]

main :: IO ()            
main = do
  gen docs
