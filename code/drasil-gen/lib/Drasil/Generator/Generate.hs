-- | Defines Drasil generator functions.
module Drasil.Generator.Generate (
  -- * Generator Functions
  gen, genDot, genCode,
  -- * Types (Printing Options)
  DocType(..), DocSpec(DocSpec), DocChoices(DC),
  Format(TeX, HTML, Jupyter, MDBook),
  -- * Constructor
  docChoices
) where

import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, render)
import Prelude hiding (id)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian)

import Build.Drasil (genMake)
import Language.Drasil
import Drasil.DocLang (mkGraphInfo)
import Drasil.System (System)
import Language.Drasil.Printers (DocType(SRS, Website, Lesson), makeCSS, genHTML,
  genTeX, Format(TeX, HTML, Jupyter, MDBook), genJupyter, genMDBook,
  PrintingInformation, outputDot, makeBook, makeRequirements)
import Language.Drasil.Code (generator, generateCode, generateCodeProc,
  Choices(..), CodeSpec(..), HasOldCodeSpec(..), Lang(..),
  getSampleData, readWithDataDesc, sampleInputDD, unPP, unJP, unCSP, unCPPP, unSP, unJLP)

import Drasil.Generator.Formats (Filename, DocSpec(DocSpec), DocChoices(DC))

import Drasil.GOOL (unJC, unPC, unCSC, unCPPC, unSC)
import Drasil.GProc (unJLC)
import Control.Lens ((^.))

import Utils.Drasil (createDirIfMissing)

-- | Generate a document in one or many flavours (HTML, TeX+Makefile,
-- mdBook+Makefile, or Jupyter Notebook, up to document type).
gen :: DocSpec -> Document -> PrintingInformation -> IO ()
gen (DocSpec (DC Lesson _) fn) body sm = prntDoc body sm fn Lesson Jupyter
gen (DocSpec (DC dt fmts) fn)  body sm = mapM_ (prntDoc body sm fn dt) fmts

-- | Helper for writing the documents (TeX / HTML / Jupyter) to file.
prntDoc :: Document -> PrintingInformation -> String -> DocType -> Format -> IO ()
prntDoc d pinfo fn Lesson Jupyter = prntDoc' Lesson "Lesson" fn Jupyter d pinfo
prntDoc _ _     _  Lesson _       =
  error "Lesson-plan rendering only supports Jupyter Notebook output type."
prntDoc d pinfo fn dtype fmt =
  case fmt of
    HTML    -> do prntDoc' dtype (show dtype ++ "/HTML") fn HTML d pinfo
                  prntCSS dtype fn d
    TeX     -> do prntDoc' dtype (show dtype ++ "/PDF") fn TeX d pinfo
                  prntMake $ DocSpec (DC dtype [TeX]) fn
    Jupyter -> do prntDoc' dtype (show dtype ++ "/Jupyter") fn Jupyter d pinfo
    MDBook  -> do prntDoc' dtype (show dtype ++ "/mdBook") fn MDBook d pinfo
                  prntMake $ DocSpec (DC dtype [MDBook]) fn
                  prntBook dtype d pinfo
                  prntCSV  dtype pinfo
    Plain   -> putStrLn "Plain-rendering is not supported."

-- | Helper function to produce an error when an incorrect SRS format is used.
srsFormatError :: a
srsFormatError = error "We can only write TeX/HTML/JSON/MDBook (for now)."

-- | Helper that takes the document type, directory name, document name, format of documents,
-- document information and printing information. Then generates the document file.
prntDoc' :: DocType -> String -> String -> Format -> Document -> PrintingInformation -> IO ()
prntDoc' _ dt' _ MDBook body' sm = do
  createDirIfMissing True dir
  mapM_ writeDocToFile con
  where
    con = writeDoc' sm MDBook body'
    dir = dt' ++ "/src"
    writeDocToFile (fp, d) = do
      outh <- openFile (dir ++ "/" ++ fp ++ ".md") WriteMode
      hPutStrLn outh $ render d
      hClose outh
prntDoc' dt dt' fn format body' sm = do
  createDirIfMissing True dt'
  outh <- openFile (dt' ++ "/" ++ fn ++ getExt format) WriteMode
  hPutStrLn outh $ render $ writeDoc sm dt format fn body'
  hClose outh
  where
    -- | Gets extension for a particular format.
    -- MDBook case is handled above.
    getExt  TeX         = ".tex"
    getExt  HTML        = ".html"
    getExt  Jupyter     = ".ipynb"
    getExt _            = srsFormatError

-- | Helper for writing the Makefile(s).
prntMake :: DocSpec -> IO ()
prntMake ds@(DocSpec (DC dt f) _) =
  do outh <- openFile (show dt ++ dir f ++ "/Makefile") WriteMode
     hPutStrLn outh $ render $ genMake [ds]
     hClose outh
  where
    dir [TeX]    = "/PDF"
    dir [MDBook] = "/mdBook"
    dir _        = error "Makefile(s) only supported for TeX/MDBook."

-- | Helper that creates a CSS file to accompany an HTML file.
-- Takes in the folder name, generated file name, and the document.
prntCSS :: DocType -> String -> Document -> IO ()
prntCSS docType fn body = do
  outh2 <- openFile (getFD docType ++ fn ++ ".css") WriteMode
  hPutStrLn outh2 $ render (makeCSS body)
  hClose outh2
  where
    getFD dtype = show dtype ++ "/HTML/"

-- | Helper for generating the .toml config file for mdBook.
prntBook :: DocType -> Document -> PrintingInformation -> IO()
prntBook dt doc sm = do
  outh <- openFile fp WriteMode
  hPutStrLn outh $ render (makeBook doc sm)
  hClose outh
  where
    fp = show dt ++ "/mdBook/book.toml"

prntCSV :: DocType -> PrintingInformation -> IO()
prntCSV dt sm = do
  outh <- openFile fp WriteMode
  hPutStrLn outh $ render (makeRequirements sm)
  hClose outh
  where
    fp = show dt ++ "/mdBook/.drasil-requirements.csv"

-- | Renders single-page documents.
writeDoc :: PrintingInformation -> DocType -> Format -> Filename -> Document -> Doc
writeDoc s _  TeX               _  doc = genTeX doc s
writeDoc s _  HTML              fn doc = genHTML s fn doc
writeDoc s dt Jupyter           _  doc = genJupyter s dt doc
writeDoc _ _  _                 _  _   = srsFormatError

-- | Renders multi-page documents.
writeDoc' :: PrintingInformation -> Format -> Document -> [(Filename, Doc)]
writeDoc' s MDBook doc = genMDBook s doc
writeDoc' _ _      _   = srsFormatError

-- | Generates traceability graphs as .dot files.
genDot :: System -> IO ()
genDot si = do
    workingDir <- getCurrentDirectory
    let gi = mkGraphInfo si
    outputDot "TraceyGraph" gi
    setCurrentDirectory workingDir

-- | Calls the code generator.
genCode :: Choices -> CodeSpec -> IO ()
genCode chs spec = do
  workingDir <- getCurrentDirectory
  time <- getCurrentTime
  sampData <- maybe (return []) (\sd -> readWithDataDesc sd $ sampleInputDD
    (spec ^. extInputsO)) (getSampleData chs)
  createDirIfMissing False "src"
  setCurrentDirectory "src"
  let genLangCode Java = genCall Java unJC unJP
      genLangCode Python = genCall Python unPC unPP
      genLangCode CSharp = genCall CSharp unCSC unCSP
      genLangCode Cpp = genCall Cpp unCPPC unCPPP
      genLangCode Swift = genCall Swift unSC unSP
      genLangCode Julia = genCallProc Julia unJLC unJLP
      genCall lng unProgRepr unPackRepr = generateCode lng unProgRepr
        unPackRepr $ generator lng (showGregorian $ utctDay time) sampData chs spec
      genCallProc lng unProgRepr unPackRepr = generateCodeProc lng unProgRepr
        unPackRepr $ generator lng (showGregorian $ utctDay time) sampData chs spec
  mapM_ genLangCode (lang chs)
  setCurrentDirectory workingDir

-- | Constructor for users to choose their document options
docChoices :: DocType -> [Format] -> DocChoices
docChoices = DC
