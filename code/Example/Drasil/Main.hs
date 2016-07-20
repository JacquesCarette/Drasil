module Main where

import Language.Drasil (DocType(SRS,MG,LPM,Website,Code))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.HGHC (srsBody,mgBody) --,lpmBody
import Example.Drasil.PCM.Body (pcm_srs)

docs :: [Recipe]
docs = [Recipe (SRS "SRS")     srsBody, 
        Recipe (Website "SRS") srsBody,
        Recipe (Website "MG") mgBody,
--      Recipe (LPM "LPM")     lpmBody,
--      Recipe (SRS "PCM_SRS") pcm_srs,
--      Recipe (Website "PCM_SRS") pcm_srs,
        Recipe (MG "MG") mgBody
       ]

main :: IO ()            
main = do
  gen docs
