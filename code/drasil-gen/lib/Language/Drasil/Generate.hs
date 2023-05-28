-- | Defines Drasil generator functions.
module Language.Drasil.Generate (
  -- * Type checking
  typeCheckSI,
  -- * Generator Functions
  gen, genDot, genCode, genLog,
  -- * Types (Printing Options)
  DocType(..), DocSpec(DocSpec), Format(TeX, HTML, JSON), DocChoices(DC),
  -- * Constructor
  docChoices) where

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
import SysInfo.Drasil (SystemInformation(SI, _sys))
import Language.Drasil.Printers (DocType(SRS, Website, Jupyter), Format(TeX, HTML, JSON),
 makeCSS, genHTML, genTeX, genJSON, PrintingInformation, outputDot, printAllDebugInfo)
import Language.Drasil.Code (generator, generateCode, Choices(..), CodeSpec(..),
  Lang(..), getSampleData, readWithDataDesc, sampleInputDD,
  unPP, unJP, unCSP, unCPPP, unSP)
import Language.Drasil.Output.Formats(Filename, DocSpec(DocSpec), DocChoices(DC))

import Language.Drasil.TypeCheck

import GOOL.Drasil (unJC, unPC, unCSC, unCPPC, unSC)
import Data.Char (isSpace)

-- | Generate a number of artifacts based on a list of recipes.
gen :: DocSpec -> Document -> PrintingInformation -> IO ()
gen ds fn sm = prnt sm ds fn -- FIXME: 'prnt' is just 'gen' with the arguments reordered

-- TODO: Include Jupyter into the SRS setup.
-- | Generate the output artifacts (TeX+Makefile, HTML or Notebook).
prnt :: PrintingInformation -> DocSpec -> Document -> IO ()
prnt sm (DocSpec (DC Jupyter _) fn) body =
  do prntDoc body sm fn Jupyter JSON
prnt sm (DocSpec (DC dtype fmts) fn) body =
  do mapM_ (prntDoc body sm fn dtype) fmts

-- | Helper for writing the documents (TeX / HTML / JSON) to file.
prntDoc :: Document -> PrintingInformation -> String -> DocType -> Format -> IO ()
prntDoc d pinfo fn Jupyter _ = prntDoc' Jupyter "Jupyter" fn JSON d pinfo
prntDoc d pinfo fn dtype fmt =
  case fmt of
    HTML -> do prntDoc' dtype (show dtype ++ "/HTML") fn HTML d pinfo
               prntCSS dtype fn d
    TeX -> do prntDoc' dtype (show dtype ++ "/PDF") fn TeX d pinfo
              prntMake $ DocSpec (DC dtype []) fn
    JSON -> do prntDoc' dtype (show dtype ++ "/JSON") fn JSON d pinfo
    _ -> mempty

-- | Helper that takes the document type, directory name, document name, format of documents,
-- document information and printing information. Then generates the document file.
prntDoc' :: DocType -> String -> String -> Format -> Document -> PrintingInformation -> IO ()
prntDoc' dt dt' fn format body' sm = do
  createDirectoryIfMissing True dt'
  outh <- openFile (dt' ++ "/" ++ fn ++ getExt format) WriteMode
  hPutStrLn outh $ render $ writeDoc sm dt format fn body'
  hClose outh
  where getExt TeX  = ".tex"
        getExt HTML = ".html"
        getExt JSON = ".ipynb"
        getExt _    = error "We can only write in TeX, HTML and Jupyter Notebook (for now)."

-- | Helper for writing the Makefile(s).
prntMake :: DocSpec -> IO ()
prntMake ds@(DocSpec (DC dt _) _) =
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
    getFD dtype = show dtype ++ "/HTML/"

-- | Renders the documents.
writeDoc :: PrintingInformation -> DocType -> Format -> Filename -> Document -> Doc
writeDoc s _  TeX  _  doc = genTeX doc s
writeDoc s _  HTML fn doc = genHTML s fn doc
writeDoc s dt JSON _  doc = genJSON s dt doc
writeDoc _ _  _    _  _   = error "we can only write TeX/HTML/JSON (for now)"

-- | Generates traceability graphs as .dot files.
genDot :: SystemInformation -> IO ()
genDot si = do
    workingDir <- getCurrentDirectory
    let gi = mkGraphInfo si
    outputDot "TraceyGraph" gi
    setCurrentDirectory workingDir

-- | Generates debugging logs to show all of the 'UID's used in an example.
genLog :: SystemInformation -> PrintingInformation -> IO ()
genLog SI{_sys = sysName} pinfo = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing True $ "../../debug/" ++ filter (not.isSpace) (abrv sysName) ++ "/SRSlogs"
  setCurrentDirectory $ "../../debug/" ++ filter (not.isSpace) (abrv sysName) ++ "/SRSlogs"
  handle <- openFile (filter (not.isSpace) (abrv sysName) ++ "_SRS.log") WriteMode
  mapM_ (hPutStrLn handle . render) $ printAllDebugInfo pinfo
  hClose handle
  setCurrentDirectory workingDir

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

-- | Constructor for users to choose their document options
docChoices :: DocType -> [Format] -> DocChoices
docChoices = DC
