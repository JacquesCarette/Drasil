module Main where

import Helpers
import System.IO
import Text.PrettyPrint.HughesPJ

data DocType = SRS
             | LPM
             | Code

data Recipe = Recipe [DocType]

gen r = do prnt SRS
           prnt LPM
  
prnt SRS = do outh <- openFile "SRS.tex" WriteMode
              hPutStrLn outh $ render $ createSRS
              hClose outh
prnt LPM = do outh <- openFile "LPM.tex" WriteMode
              hPutStrLn outh $ render $ createLPM
              hClose outh
prnt _ = error "Invalid recipe"

auth = "Spencer Smith"


spre = docclass [] "article" $$ usepackage "longtable" $$ usepackage "booktabs"
lpre = docclass "article" "cweb-hy" $$ usepackage "xr" $$ exdoc "-L" "hghc_SRS"

createSRS :: Doc  
createSRS = spre $$ title "Literate Programmer's Manual for $h_g$ and $h_c$" $$
            author auth $$ begin

createLPM :: Doc
createLPM = lpre $$ title "Literate Programmer's Manual for $h_g$ and $h_c$" $$
            author auth $$ begin
  
main = do
  gen (Recipe [SRS, LPM])