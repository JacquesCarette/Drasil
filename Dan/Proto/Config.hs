{-# OPTIONS -Wall #-} 
module Config where
import ASTInternal

output :: OutFormat  
output = TeX

outLang :: OutLang
outLang = CLang

precision :: Precision
precision = Double

expandSymbols :: Bool
expandSymbols = True