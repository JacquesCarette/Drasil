{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal (DocType(SRS)) --,LPM))
import Body1 (srsBody) --,lpmBody)
import Gen (Recipe(..), writeDoc, gen)
import Text.PrettyPrint
import Format (TeX(..))

docs :: [Recipe]
docs = [Recipe SRS "SRS.tex" createSRS 
--        Recipe SRS "PCM_SRS.tex" createSRS2,
--        Recipe LPM "LPM.w" createLPM
       ]
       
--generation functions
createSRS :: Doc
createSRS = writeDoc TeX SRS srsBody

--createLPM :: Doc  
--createLPM = writeDoc LPM lpmBody
       
main :: IO ()            
main = do
  putStrLn $ "Everything is broken! Woohoo!"
  gen docs
