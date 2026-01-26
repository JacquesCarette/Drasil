module Drasil.Generator.SRS (
  -- * Generators
  exportSmithEtAlSrs
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, render)

import Build.Drasil (genMake)
import Drasil.DocLang (mkGraphInfo)
import Language.Drasil (Stage(Equational), Document(Document, Notebook),
  ShowTableOfContents, checkToC)
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Printers (makeCSS, makeRequirements, genHTML, genTeX,
  genMDBook, makeBook, defaultConfiguration, piSys, PrintingInformation,
  genJupyterSRS)
import Drasil.SRSDocument (SRSDecl, mkDoc)
import Language.Drasil.Printing.Import (makeDocument, makeProject)
import Drasil.System (System, refTable, systemdb)
import Utils.Drasil (createDirIfMissing)

import Drasil.Generator.ChunkDump (dumpEverything)
import Drasil.Generator.Formats (DocSpec(..), DocChoices(DC), Filename,
  docChoices, Format(..), DocType(SRS))
import Drasil.Generator.SRS.TraceabilityGraphs (outputDot)
import Drasil.Generator.SRS.TypeCheck (typeCheckSI)

-- | Generate an SRS softifact.
exportSmithEtAlSrs :: System -> SRSDecl -> String -> IO ()
exportSmithEtAlSrs syst srsDecl srsFileName = do
  let (srs, syst') = mkDoc syst srsDecl S.forT
      printfo = piSys (syst' ^. systemdb) (syst' ^. refTable) Equational defaultConfiguration
  dumpEverything syst' printfo ".drasil/"
  typeCheckSI syst' -- FIXME: This should be done on `System` creation *or* chunk creation!
  genDoc (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) srsFileName) srs printfo
  genDot syst' -- FIXME: This *MUST* use syst', NOT syst (or else it misses things!)!

-- | Generate a document in one or many flavours (HTML, TeX+Makefile,
-- mdBook+Makefile, or Jupyter Notebook, up to document type).
genDoc :: DocSpec -> Document -> PrintingInformation -> IO ()
genDoc (DocSpec (DC dt fmts) fn)  body sm = mapM_ (prntDoc body sm fn dt) fmts

-- | Helper for writing the documents (TeX / HTML / Jupyter) to file.
prntDoc :: Document -> PrintingInformation -> String -> DocType -> Format -> IO ()
prntDoc d pinfo fn dtype fmt =
  case fmt of
    HTML    -> do prntDoc' (show dtype ++ "/HTML") fn HTML d pinfo
                  prntCSS dtype fn d
    TeX     -> do prntDoc' (show dtype ++ "/PDF") fn TeX d pinfo
                  prntMake $ DocSpec (DC dtype [TeX]) fn
    Jupyter -> do prntDoc' (show dtype ++ "/Jupyter") fn Jupyter d pinfo
    MDBook  -> do prntDoc' (show dtype ++ "/mdBook") fn MDBook d pinfo
                  prntMake $ DocSpec (DC dtype [MDBook]) fn
                  prntBook dtype d pinfo
                  prntCSV  dtype pinfo
    Plain   -> putStrLn "Plain-rendering is not supported."

-- | Common error for when an unsupported SRS format is attempted.
srsFormatError :: a
srsFormatError = error "We can only write TeX/HTML/JSON/MDBook (for now)."

-- | Helper that takes the document type, directory name, document name, format of documents,
-- document information and printing information. Then generates the document file.
prntDoc' :: String -> String -> Format -> Document -> PrintingInformation -> IO ()
prntDoc' dt' _ MDBook body' sm = do
  createDirIfMissing True dir
  mapM_ writeDocToFile con
  where
    con = writeDoc' sm MDBook body'
    dir = dt' ++ "/src"
    writeDocToFile (fp, d) = do
      outh <- openFile (dir ++ "/" ++ fp ++ ".md") WriteMode
      hPutStrLn outh $ render d
      hClose outh
prntDoc' dt' fn format body' sm = do
  createDirIfMissing True dt'
  outh <- openFile (dt' ++ "/" ++ fn ++ getExt format) WriteMode
  hPutStrLn outh $ render $ writeDoc sm format fn body'
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
writeDoc :: PrintingInformation -> Format -> Filename -> Document -> Doc
writeDoc s  TeX     _  doc = genTeX (makeDocument s dd) mToC s
  where
    getDoc :: Document -> (Document, ShowTableOfContents)
    getDoc d@(Document _ _ st _) = (d , st)
    getDoc   (Notebook{})        = error "cannot render notebooks into LaTeX"
    (dd , mToC) = getDoc $ checkToC doc
writeDoc s  HTML    fn doc = genHTML fn $ makeDocument s doc
writeDoc s Jupyter _  doc = genJupyterSRS $ makeDocument s doc
writeDoc _  _       _  _   = srsFormatError

-- | Renders multi-page documents.
writeDoc' :: PrintingInformation -> Format -> Document -> [(Filename, Doc)]
writeDoc' s MDBook doc = genMDBook $ makeProject s doc
writeDoc' _ _      _   = srsFormatError

-- | Generates traceability graphs as .dot files.
genDot :: System -> IO ()
genDot si = do
    workingDir <- getCurrentDirectory
    let gi = mkGraphInfo si
    outputDot "TraceyGraph" gi
    setCurrentDirectory workingDir
