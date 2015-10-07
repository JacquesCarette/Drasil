{-# OPTIONS -Wall #-} 
module Main where
import System.IO
import Text.PrettyPrint.HughesPJ
import Body
import TeXHelpers
import qualified Body2 as E2

data DocType = SRS
             | LPM
             | Code

data Recipe = Recipe (DocType, String, Doc)
        --DocType, Filename, 'Body'

gen :: [Recipe] -> IO ()
gen ((Recipe (x,y,z)):[]) = do prnt x y z
gen ((Recipe (x,y,z)):xs) = do prnt x y z
                               gen xs
gen ([])                  = return ()

prnt :: DocType -> String -> Doc -> IO ()  
prnt SRS filename body = do outh <- openFile filename WriteMode
                            hPutStrLn outh $ render $ body
                            hClose outh
prnt LPM filename body = do outh <- openFile filename WriteMode
                            hPutStrLn outh $ render $ body
                            hClose outh
  -- No difference b/w SRS and LPM as yet
prnt Code _ _ = error "Code DocType is not implemented yet"

auth :: String
auth = "Spencer Smith"

auth2 :: String
auth2 = "Thulasi Jegatheesan"

spre,lpre :: Doc
spre = docclass [] "article" $$ usepackage "longtable" $$ usepackage "booktabs"
lpre = docclass "article" "cweb-hy" $$ usepackage "xr" $$ exdoc "L-" "hghc_SRS"

createSRS :: Doc  
createSRS = spre $$ title "SRS for $h_g$ and $h_c$" $$
            author auth $$ srsComms $$ begin $$ srsBody $$ end
            
createLPM :: Doc
createLPM = lpre $$ title "Literate Programmer's Manual for $h_g$ and $h_c$" $$
            author auth $$ lpmComms $$ begin $$ lpmBody $$ endL

createSRS2 :: Doc  
createSRS2 = spre $$ title ("Software Requirements Specification for Solar " ++ 
             "Water Heating Systems Incorporating Phase Change Material") $$
             author auth2 $$ srsComms $$ begin $$ E2.srsBody $$ end

docs :: [Recipe]
docs = [Recipe (SRS, "SRS.tex", createSRS), 
        Recipe (SRS, "PCM_SRS.tex", createSRS2),
        Recipe (LPM, "LPM.w", createLPM)]
             
main :: IO ()            
main = do
  gen docs
