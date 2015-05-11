module Config where

import Helpers
import Text.PrettyPrint.HughesPJ

-- Macro / Command def'n --
--TeX--
srsComms = bullet $$ counter $$ ddefnum $$ ddref $$ colAw $$ colBw $$ arrayS
lpmComms = text ""

bullet = comm "blt" "- " []
counter = count "datadefnum"
ddefnum = comm "ddthedatadefnum" "DD\\thedatadefnum" []
ddref = comm "ddref" "DD\\ref{#1}" "1"
colAw = comm "colAwidth" "0.2\\textwidth" []
colBw = comm "colBwidth" "0.73\\textwidth" []
arrayS = renewcomm "arraystretch" "1.2"

--Could config chunk records here--
data Field = Symbol | Equation | Description | SIU
  deriving (Ord, Eq)

output :: OutFormat  
output = TeX
