{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal_MK2
import Config_MK2
import Body1_MK2 (srsBody,lpmBody)
import Gen_MK2
import Text.PrettyPrint

docs :: [Recipe]
docs = [Recipe (SRS, "SRS.tex", createSRS), 
--        Recipe (SRS, "PCM_SRS.tex", createSRS2),
        Recipe (LPM, "LPM.w", createLPM)
       ]
       
--generation functions
createSRS,createLPM :: Doc  
createSRS = (writeDoc output SRS) srsBody 

createLPM = (writeDoc output LPM) lpmBody
       
main :: IO ()            
main = do
  gen docs
