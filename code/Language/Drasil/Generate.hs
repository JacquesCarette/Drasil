{-# OPTIONS -Wall #-} 
module Language.Drasil.Generate (gen) where

import System.IO
import Text.PrettyPrint.HughesPJ

import Language.Drasil.Output.Formats (DocType (SRS,LPM,Code,Website))
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.HTML.Helpers (makeCSS)
import Language.Drasil.Document (Document)
import Language.Drasil.Format(Format(TeX, HTML))
import Language.Drasil.Recipe(Recipe(Recipe))

-- Generate a number of artifacts based on a list of recipes.
gen :: [Recipe] -> IO ()
gen rl = mapM_ prnt rl

prnt :: Recipe -> IO ()
prnt (Recipe (SRS fn) body) = 
  do outh <- openFile (fn ++ ".tex") WriteMode
     hPutStrLn outh $ render $ (writeDoc TeX (SRS fn) body)
     hClose outh
prnt (Recipe (LPM fn) body) = 
  do outh <- openFile (fn ++ ".w") WriteMode
     hPutStrLn outh $ render $ (writeDoc TeX (LPM fn) body)
     hClose outh
prnt (Recipe (Website fn) body) =
  do outh <- openFile (fn ++ ".html") WriteMode
     hPutStrLn outh $ render $ (writeDoc HTML (Website fn) body)
     hClose outh
     outh2 <- openFile (fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2
prnt (Recipe (Code _) _) = error "Code DocType is not implemented yet"

writeDoc :: Format -> DocType -> Document -> Doc
writeDoc TeX  = genTeX
writeDoc HTML = genHTML
writeDoc _    = error "we can only write TeX/HTML (for now)"
