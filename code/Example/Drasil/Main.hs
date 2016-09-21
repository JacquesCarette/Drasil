module Main where

import Language.Drasil (DocType(SRS,MG,MIS,Website),Recipe(..),gen)

import Drasil.HGHC (srsBody,mgBody,misBody) --,lpmBody
import Drasil.PCM.Body (pcm_srs)

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
