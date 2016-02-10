{-# OPTIONS -Wall #-} 
module Gen where
import System.IO
import Text.PrettyPrint.HughesPJ
import PrintTeX (genTeX)
import ASTInternal (DocType (SRS,LPM,Code))
import Spec (Document)
import Format(Format(TeX))

data Recipe = Recipe DocType String Doc
                  --DocType, Filename, 'Body'

-- Generate a number of artifacts based on a list of recipes.
gen :: [Recipe] -> IO ()
gen rl = mapM_ prnt rl

prnt :: Recipe -> IO ()
prnt (Recipe SRS filename body) = 
  do outh <- openFile filename WriteMode
     hPutStrLn outh $ render $ body
     hClose outh
prnt (Recipe LPM filename body) = 
  do outh <- openFile filename WriteMode
     hPutStrLn outh $ render $ body
     hClose outh
  -- No difference b/w SRS and LPM as yet, but there may be in the future so
  -- keeping them separate for the time being is a good idea.
  -- Looking more and more like LPM might become radically different (or 
  -- removed) in future iterations. Definitely will not be creating .w file.
prnt (Recipe Code _ _) = error "Code DocType is not implemented yet"

writeDoc :: Format -> DocType -> Document -> Doc
writeDoc TeX = genTeX
writeDoc _ = error "we can only write TeX (for now)"
