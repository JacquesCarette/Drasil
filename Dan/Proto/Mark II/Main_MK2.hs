{-# OPTIONS -Wall #-} 
module Main where
import ASTInternal_MK2
import Config_MK2
import Body1_MK2 (srsBody)
import Gen_MK2
import Text.PrettyPrint

-- spre,lpre :: Doc
-- spre = docclass [] "article" $$ usepackage "longtable" $$ usepackage "booktabs"
-- lpre = docclass "article" "cweb-hy" $$ usepackage "xr" $$ exdoc "L-" "hghc_SRS"

docs :: [Recipe]
docs = [Recipe (SRS, "SRS.tex", createSRS) --, 
--        Recipe (SRS, "PCM_SRS.tex", createSRS2),
--        Recipe (LPM, "LPM.w", createLPM)
       ]
       
--generation functions
createSRS :: Doc  
createSRS = (writeDoc output SRS) srsBody 

       
main :: IO ()            
main = do
  gen docs
