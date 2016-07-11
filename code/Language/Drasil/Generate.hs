{-# OPTIONS -Wall #-} 
module Language.Drasil.Generate (gen) where

import System.IO
import Text.PrettyPrint.HughesPJ

import Control.Lens ((^.))
import System.Directory
import Language.Drasil.Output.Formats (DocType (SRS,MG,LPM,Code,Website))
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.HTML.Helpers (makeCSS)
import Language.Drasil.CCode.Print (genCode)
import Language.Drasil.Make.Print (genMake)
import Language.Drasil.Document (Document(..), LayoutObj(..))
import Language.Drasil.Format(Format(TeX, HTML))
import Language.Drasil.Recipe(Recipe(Recipe))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module
import Language.Drasil.Config (outLang)

-- Generate a number of artifacts based on a list of recipes.
gen :: [Recipe] -> IO ()
gen rl =
  do mapM_ prnt rl
     prntMake rl

prnt :: Recipe -> IO ()
prnt (Recipe (SRS fn) body) = 
  do createDirectoryIfMissing False "SRS"
     outh <- openFile ("SRS/" ++ fn ++ ".tex") WriteMode
     hPutStrLn outh $ render $ (writeDoc TeX (SRS fn) body)
     hClose outh
prnt (Recipe (MG fn) body) =
  do createDirectoryIfMissing False "MG"
     outh <- openFile ("MG/" ++ fn ++ ".tex") WriteMode
     hPutStrLn outh $ render $ (writeDoc TeX (MG fn) body)
     hClose outh
     prntCode body
prnt (Recipe (LPM fn) body) = 
  do createDirectoryIfMissing False "LPM"
     outh <- openFile ("LPM/" ++ fn ++ ".w") WriteMode
     hPutStrLn outh $ render $ (writeDoc TeX (LPM fn) body)
     hClose outh
prnt (Recipe (Website fn) body) =
  do createDirectoryIfMissing False "Website"
     outh <- openFile ("Website/" ++ fn ++ ".html") WriteMode
     hPutStrLn outh $ render $ (writeDoc HTML (Website fn) body)
     hClose outh
     outh2 <- openFile ("Website/" ++ fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2
prnt (Recipe (Code _) _) = error "Code DocType is not implemented yet"

prntCode :: Document -> IO ()
prntCode (Document _ _ los) = mapM_ prntCode' (getModules los)
  where   getModules []                 = []
          getModules ((Module m):los)   = (genCode outLang m) ++ getModules los
          getModules (_:los)            = getModules los
          prntCode' (name, code)        = do createDirectoryIfMissing False "Code"
                                             outh <- openFile ("Code/" ++ name) WriteMode
                                             hPutStrLn outh $ render $ code
                                             hClose outh


prntMake :: [Recipe] -> IO ()
prntMake rl =
  do outh <- openFile ("Makefile") WriteMode
     hPutStrLn outh $ render $ genMake (map (\(Recipe x _) -> x) rl)
     hClose outh

writeDoc :: Format -> DocType -> Document -> Doc
writeDoc TeX  = genTeX
writeDoc HTML = genHTML
writeDoc _    = error "we can only write TeX/HTML (for now)"
