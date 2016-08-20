module Main where

import Language.Drasil (DocType(SRS,MG,MIS,LPM,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.HGHC (srsBody,mgBody,misBody) --,lpmBody
import Example.Drasil.PCM.Body (pcm_srs)

docs :: [Recipe]
docs = [Recipe (Website "SRS") srsBody,
        Recipe (Website "MG") mgBody,
        Recipe (Website "MIS") misBody,
        Recipe (SRS "SRS") srsBody,
        Recipe (MG "MG") mgBody,
        Recipe (MIS "MIS") misBody,
--      Recipe (LPM "LPM")     lpmBody,
        Recipe (SRS "PCM_SRS") pcm_srs,
        Recipe (Website "PCM_SRS") pcm_srs
       ]

main :: IO ()            
main = do
  gen docs
