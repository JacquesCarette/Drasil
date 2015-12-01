{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal (DocType(SRS)) --,LPM))
import Body1 (srsBody) --,lpmBody)
import Gen (Recipe(..), writeDoc, gen)
import Format(Format(TeX))

import Text.PrettyPrint
import Format (Format(TeX))

docs :: [Recipe]
docs = [Recipe SRS "SRS.tex" createSRS, 
--        Recipe SRS "PCM_SRS.tex" createSRS2,
        Recipe LPM "LPM.w" createLPM
       ]
       
--generation functions
createSRS,createLPM :: Doc  
createSRS = writeDoc TeX SRS srsBody
createLPM = writeDoc TeX LPM lpmBody
createSRS :: Doc
createSRS = writeDoc TeX SRS srsBody

main :: IO ()            
main = do
  gen docs
