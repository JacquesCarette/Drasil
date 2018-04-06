module Language.Drasil.Generate (gen, genCode) where

import System.IO
import Text.PrettyPrint.HughesPJ
import Prelude hiding (id)
import System.Directory

import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,Website),DocSpec(DocSpec),Filename)
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.HTML.Helpers (makeCSS)
import Language.Drasil.Make.Print (genMake)
import Language.Drasil.Document
import Language.Drasil.Format(Format(TeX, HTML))
import Language.Drasil.Code.Imperative.Import (generator, generateCode)
import Language.Drasil.CodeSpec
import Language.Drasil.ChunkDB (HasSymbolTable(..))

-- | Generate a number of artifacts based on a list of recipes.
gen :: HasSymbolTable s => DocSpec -> Document -> s -> IO ()
gen ds fn sm = prnt sm ds fn

-- | Generate the output artifacts (TeX+Makefile or HTML)
prnt :: HasSymbolTable s => s -> DocSpec -> Document -> IO ()
prnt sm dt@(DocSpec Website fn) body =
  do prntDoc dt body sm
     outh2 <- openFile ("Website/" ++ fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2
prnt sm dt@(DocSpec _ _) body =
  do prntDoc dt body sm
     prntMake dt

-- | Helper for writing the documents (TeX / HTML) to file
prntDoc :: HasSymbolTable s => DocSpec -> Document -> s -> IO ()
prntDoc (DocSpec dt fn) body sm = prntDoc' dt fn (fmt dt) body sm
  where fmt SRS = TeX
        fmt MG  = TeX
        fmt MIS = TeX
        fmt Website = HTML

prntDoc' :: (HasSymbolTable s, Show a) => a -> String -> Format -> Document -> s -> IO ()
prntDoc' dt' fn format body' sm = do
  createDirectoryIfMissing False $ show dt'
  outh <- openFile (show dt' ++ "/" ++ fn ++ getExt format) WriteMode
  hPutStrLn outh $ render $ writeDoc format fn body' sm
  hClose outh
  where getExt TeX  = ".tex"
        getExt HTML = ".html"
        getExt _    = error "we can only write TeX/HTML (for now)"

-- | Helper for writing the Makefile(s)
prntMake :: DocSpec -> IO ()
prntMake ds@(DocSpec dt _) =
  do outh <- openFile (show dt ++ "/Makefile") WriteMode
     hPutStrLn outh $ render $ genMake [ds]
     hClose outh

-- | Renders the documents
writeDoc :: HasSymbolTable s => Format -> Filename -> Document -> s -> Doc
writeDoc TeX  _  doc s = genTeX doc s
writeDoc HTML fn doc s = genHTML fn doc s
writeDoc _    _  _   _ = error "we can only write TeX/HTML (for now)"

-- | Calls the code generator
genCode :: Choices -> CodeSpec -> IO ()
genCode ch spec = do 
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "src"
  setCurrentDirectory "src"
  generateCode ch $ generator ch spec
  setCurrentDirectory workingDir
