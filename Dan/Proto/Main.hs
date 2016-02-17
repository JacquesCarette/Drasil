{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal (DocType(SRS,LPM,Website))
import Body1 (srsBody,lpmBody)
import Gen (Recipe(..), gen)

docs :: [Recipe]
docs = [Recipe (SRS "SRS")     srsBody, 
        Recipe (Website "SRS") srsBody,
--        Recipe SRS "PCM_SRS.tex" createSRS2,
        Recipe (LPM "LPM")     lpmBody
       ]

main :: IO ()            
main = do
  gen docs
