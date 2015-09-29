{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal (DocType(..))
import Config_MK2 (output)
import Body1 (srsBody,lpmBody)
import Gen_MK2 (Recipe(..), writeDoc, gen)
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
