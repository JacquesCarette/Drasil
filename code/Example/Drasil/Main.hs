module Main where

import Language.Drasil (DocType(SRS,LPM,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.HGHC (srsBody) --,lpmBody
import Example.Drasil.PCM.Body (pcm_srs)

docs :: [Recipe]
docs = [Recipe (SRS "SRS")     srsBody, 
        Recipe (Website "SRS") srsBody,
        -- Recipe (LPM "LPM")     lpmBody,
        Recipe (SRS "PCM_SRS") pcm_srs,
        Recipe (Website "PCM_SRS") pcm_srs
       ]

main :: IO ()            
main = do
  gen docs
