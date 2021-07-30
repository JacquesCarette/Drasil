module Language.Drasil.Generate (gen, genDot, genCode, DocType(SRS, Website), DocSpec(DocSpec), Format(TeX, HTML)) where

import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, render)
import Prelude hiding (id)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory,
  setCurrentDirectory)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian)

import Build.Drasil (genMake)
import Language.Drasil
import Drasil.DocLang (mkGraphInfo)
import Database.Drasil (SystemInformation)
import Language.Drasil.Printers (Format(TeX, HTML), 
 makeCSS, genHTML, genTeX, PrintingInformation, outputDot)
import Language.Drasil.Code (generator, generateCode, Choices(..), CodeSpec(..),
  Lang(..), getSampleData, readWithDataDesc, sampleInputDD, 
  unPP, unJP, unCSP, unCPPP, unSP)
import Language.Drasil.Output.Formats( DocType(SRS, Website), Filename, DocSpec(DocSpec))

import GOOL.Drasil (unJC, unPC, unCSC, unCPPC, unSC)

-- | Generate a number of artifacts based on a list of recipes.
gen :: DocSpec -> Document -> PrintingInformation -> IO ()
gen ds fn sm = prnt sm ds fn

-- | Generate the output artifacts (TeX+Makefile or HTML).
prnt :: PrintingInformation -> DocSpec -> Document -> IO ()
prnt sm dt@(DocSpec Website fn) body =
  do prntDoc dt body sm
     prntCSS Website fn body
prnt sm dt@(DocSpec (SRS (x:xs)) fn) body = 
  do prntDoc (DocSpec (SRS [x]) fn) body sm
     prntAuxFiles
     prnt sm (DocSpec (SRS xs) fn) body
  where
    prntAuxFiles = case x of
      TeX  -> prntMake dt
      HTML -> prntCSS (SRS []) fn body
      _ -> mempty
prnt _ (DocSpec _ _) _ = mempty
  

-- | Helper for writing the documents (TeX / HTML) to file.
prntDoc :: DocSpec -> Document -> PrintingInformation -> IO ()
prntDoc (DocSpec Website fn) d pinfo = prntDoc' "Website" fn HTML d pinfo
prntDoc (DocSpec (SRS [HTML]) fn) d pinfo = prntDoc' "SRS/HTML" fn HTML d pinfo
prntDoc (DocSpec (SRS [TeX]) fn) d pinfo = prntDoc' "SRS/PDF" fn TeX d pinfo
prntDoc (DocSpec _ _) _ _ = error "Something is wrong in prntDoc. This should not happen."

-- | Helper that takes the directory name, document name, format of documents,
-- document information and printing information. Then generates the document file.
prntDoc' :: String -> String -> Format -> Document -> PrintingInformation -> IO ()
prntDoc' dt' fn format body' sm = do
  createDirectoryIfMissing True dt'
  outh <- openFile (dt' ++ "/" ++ fn ++ getExt format) WriteMode
  hPutStrLn outh $ render $ writeDoc sm format fn body'
  hClose outh
  where getExt TeX  = ".tex"
        getExt HTML = ".html"
        getExt _    = error "we can only write TeX/HTML (for now)"

-- | Helper for writing the Makefile(s).
prntMake :: DocSpec -> IO ()
prntMake ds@(DocSpec dt _) =
  do outh <- openFile (show dt ++ "/PDF/Makefile") WriteMode
     hPutStrLn outh $ render $ genMake [ds]
     hClose outh

-- | Helper that creates a CSS file to accompany an HTML file.
-- Takes in the folder name, generated file name, and the document.
prntCSS :: DocType -> String -> Document -> IO ()
prntCSS docType fn body = do
  outh2 <- openFile (getFD docType ++ fn ++ ".css") WriteMode
  hPutStrLn outh2 $ render (makeCSS body)
  hClose outh2
  where
    getFD Website = "Website/"
    getFD (SRS _) = "SRS/HTML/"

-- | Renders the documents.
writeDoc :: PrintingInformation -> Format -> Filename -> Document -> Doc
writeDoc s TeX  _  doc = genTeX doc s
writeDoc s HTML fn doc = genHTML s fn doc
writeDoc _    _  _   _ = error "we can only write TeX/HTML (for now)"

-- | Generates traceability graphs as .dot files.
genDot :: SystemInformation -> IO ()
genDot si = do
    let gi = mkGraphInfo si
    outputDot "TraceyGraph" gi
    return mempty

-- | Calls the code generator.
genCode :: Choices -> CodeSpec -> IO ()
genCode chs spec = do 
  workingDir <- getCurrentDirectory
  time <- getCurrentTime
  sampData <- maybe (return []) (\sd -> readWithDataDesc sd $ sampleInputDD 
    (extInputs spec)) (getSampleData chs)
  createDirectoryIfMissing False "src"
  setCurrentDirectory "src"
  let genLangCode Java = genCall Java unJC unJP
      genLangCode Python = genCall Python unPC unPP
      genLangCode CSharp = genCall CSharp unCSC unCSP
      genLangCode Cpp = genCall Cpp unCPPC unCPPP
      genLangCode Swift = genCall Swift unSC unSP
      genCall lng unProgRepr unPackRepr = generateCode lng unProgRepr 
        unPackRepr $ generator lng (showGregorian $ utctDay time) sampData chs spec
  mapM_ genLangCode (lang chs)
  setCurrentDirectory workingDir
