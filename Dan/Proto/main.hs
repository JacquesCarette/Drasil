module Main where

import Helpers
import System.IO
import Text.PrettyPrint.HughesPJ
import Config
import Body
import Chunk
import H_g

data DocType = SRS
             | LPM
             | Code

data Recipe = Recipe [DocType]

gen (Recipe (x:[])) = do prnt x
gen (Recipe (x:xs)) = do prnt x
                         gen $ Recipe xs
gen _ = error "Invalid Recipe"
  
prnt SRS = do outh <- openFile "SRS.tex" WriteMode
              hPutStrLn outh $ render $ createSRS
              hClose outh
prnt LPM = do outh <- openFile "LPM.w" WriteMode
              hPutStrLn outh $ render $ createLPM
              hClose outh
prnt _ = error "Invalid DocType"

auth = "Spencer Smith"

spre = docclass [] "article" $$ usepackage "longtable" $$ usepackage "booktabs"
lpre = docclass "article" "cweb-hy" $$ usepackage "xr" $$ exdoc "-L" "hghc_SRS"

createSRS :: Doc  
createSRS = spre $$ title "SRS for $h_g$ and $h_c$" $$
            author auth $$ srsComms $$ begin $$ srsBody $$ end

createLPM :: Doc
createLPM = lpre $$ title "Literate Programmer's Manual for $h_g$ and $h_c$" $$
            author auth $$ lpmComms $$ begin $$ lpmBody $$ end
  
main = do
  gen (Recipe [SRS, LPM])