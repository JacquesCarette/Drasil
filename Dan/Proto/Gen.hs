{-# OPTIONS -Wall #-} 
module Gen where
import System.IO
import Text.PrettyPrint.HughesPJ
import PrintTeX (genTeX)
import ASTInternal (Document, DocType (SRS,LPM,Code))
import Format(Format, TeX)

data Recipe = Recipe DocType String Doc
        --DocType, Filename, 'Body'

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
  -- keeping them separate for the time being is a good idea
prnt (Recipe Code _ _) = error "Code DocType is not implemented yet"

class Format a => DocWriter a where
   writeDoc :: DocType -> Document a -> Doc

instance DocWriter TeX where
  writeDoc = genTeX
