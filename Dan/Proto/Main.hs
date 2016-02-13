{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal (DocType(SRS,LPM,Website))
import Body1 (srsBody,lpmBody,srsBodyHTMLTest)
import Gen (Recipe(..), writeDoc, gen)
import Format(Format(TeX, HTML))
import Text.PrettyPrint

docs :: [Recipe]
docs = [Recipe (SRS "SRS") srsBody, 
        Recipe (Website "SRS") srsBodyHTMLTest,
--        Recipe SRS "PCM_SRS.tex" createSRS2,
        Recipe (LPM "LPM") lpmBody
       ]

main :: IO ()            
main = do
  gen docs
