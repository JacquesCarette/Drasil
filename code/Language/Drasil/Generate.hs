{-# OPTIONS -Wall #-} 
module Language.Drasil.Generate (gen) where

import System.IO
import Text.PrettyPrint.HughesPJ

import Control.Lens ((^.))
import System.Directory
import Language.Drasil.Output.Formats (DocType (SRS,MG,MIS,LPM,Website))
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.HTML.Helpers (makeCSS)
import Language.Drasil.CCode.Print (genCode)
import Language.Drasil.Make.Print (genMake)
import Language.Drasil.Document
import Language.Drasil.Format(Format(TeX, HTML))
import Language.Drasil.Recipe(Recipe(Recipe))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module
import Language.Drasil.Config (outLang)

-- Generate a number of artifacts based on a list of recipes.
gen :: [Recipe] -> IO ()
gen rl = mapM_ prnt rl

prnt :: Recipe -> IO ()
prnt (Recipe dt@(SRS fn) body) =
  do prntDoc dt body
     prntMake dt
prnt (Recipe dt@(MG fn) body) =
  do prntDoc dt body
     prntMake dt
     prntCode body
prnt (Recipe dt@(MIS fn) body) =
  do prntDoc dt body
     prntMake dt
prnt (Recipe dt@(LPM fn) body) =
  do prntDoc dt body
prnt (Recipe dt@(Website fn) body) =
  do prntDoc dt body
     outh2 <- openFile ("Website/" ++ fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2

prntDoc :: DocType -> Document -> IO ()
prntDoc dt body = case dt of
  (SRS fn)     -> prntDoc' dt fn TeX body
  (MG fn)      -> prntDoc' dt fn TeX body
  (MIS fn)     -> prntDoc' dt fn TeX body
  (LPM fn)     -> prntDoc' dt fn TeX body
  (Website fn) -> prntDoc' dt fn HTML body
  where prntDoc' dt fn format body = do
          createDirectoryIfMissing False $ show dt
          outh <- openFile (show dt ++ "/" ++ fn ++ getExt format) WriteMode
          hPutStrLn outh $ render $ (writeDoc format dt body)
          hClose outh
          where getExt TeX = ".tex"
                getExt HTML = ".html"


prntCode :: Document -> IO ()
prntCode (Document _ _ secs) = mapM_ prntCode'
  (concat (map (\(Section _ _ s) -> getModules s) secs))
  where getModules []                 = []
        getModules ((Con (Module m)):los) = if null (method m)
          then getModules los
          else (genCode outLang m) ++ getModules los
        getModules (_:los)     = getModules los
        prntCode' (name, code) = do createDirectoryIfMissing False "Code"
                                    outh <- openFile ("Code/" ++ name) WriteMode
                                    hPutStrLn outh $ render $ code
                                    hClose outh


prntMake :: DocType -> IO ()
prntMake dt =
  do outh <- openFile (show dt ++ "/Makefile") WriteMode
     hPutStrLn outh $ render $ genMake [dt]
     hClose outh

writeDoc :: Format -> DocType -> Document -> Doc
writeDoc TeX  = genTeX
writeDoc HTML = genHTML
writeDoc _    = error "we can only write TeX/HTML (for now)"
