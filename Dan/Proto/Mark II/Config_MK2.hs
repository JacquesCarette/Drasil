{-# OPTIONS -Wall #-} 
module Config_MK2 where
import ASTInternal_MK2

output :: OutFormat  
output = TeX

outLang :: OutLang
outLang = CLang

precision :: Precision
precision = Double

expandSymbols :: Bool
expandSymbols = True