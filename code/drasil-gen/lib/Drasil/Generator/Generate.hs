-- | Defines Drasil generator functions.
module Drasil.Generator.Generate (
  -- * Generators
  exportSmithEtAlSrs, exportLessonPlan, exportWebsite,
  exportSmithEtAlSrsWCode, exportSmithEtAlSrsWCodeZoo,
  -- * Internal Functions
  codedDirName
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, render)

import Build.Drasil (genMake)
import Drasil.DocLang (mkGraphInfo)
import Drasil.DocumentLanguage.Notebook (LsnDesc, mkNb)
import Drasil.GOOL (unJC, unPC, unCSC, unCPPC, unSC, CodeType(..))
import Drasil.GProc (unJLC)
import Language.Drasil (Stage(Equational), Document, Space(..))
import Language.Drasil.Code
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Printers (DocType(..), makeCSS, Format(..),
  makeRequirements, genHTML, genTeX, genJupyter, genMDBook, outputDot, makeBook)
import Drasil.SRSDocument (SRSDecl, defaultConfiguration, piSys,
  PrintingInformation, mkDoc)
import Language.Drasil.Printing.Import (makeDocument, makeProject)
import Drasil.System (System, programName, refTable, systemdb)
import Utils.Drasil (createDirIfMissing)
import Drasil.Generator.ChunkDump (dumpEverything)
import Drasil.Generator.Formats (Filename, DocSpec(DocSpec), DocChoices(DC), docChoices)
import Drasil.Generator.TypeCheck (typeCheckSI)

-- | Generate an SRS softifact.
exportSmithEtAlSrs :: System -> SRSDecl -> String -> IO ()
exportSmithEtAlSrs syst srsDecl srsFileName = do
  let (srs, syst') = mkDoc syst srsDecl S.forT
      printfo = piSys (syst' ^. systemdb) (syst' ^. refTable) Equational defaultConfiguration
  dumpEverything syst' printfo ".drasil/"
  typeCheckSI syst' -- FIXME: This should be done on `System` creation *or* chunk creation!
  genDoc (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) srsFileName) srs printfo
  genDot syst' -- FIXME: This *MUST* use syst', NOT syst (or else it misses things!)!

-- | Internal: Generate an ICO-style executable softifact.
exportCode :: System -> Choices -> [Mod] -> IO ()
exportCode syst chcs extraModules = do
  let code = codeSpec syst chcs extraModules
  genCode chcs code

-- | Internal: Generate a zoo of ICO-style executable softifact.
exportCodeZoo :: System -> [(Choices, [Mod])] -> IO ()
exportCodeZoo syst = mapM_ $ \(chcs, mods) -> do
  let dir = map toLower $ codedDirName (syst ^. programName) chcs
  workingDir <- getCurrentDirectory
  createDirIfMissing False dir
  setCurrentDirectory dir
  exportCode syst chcs mods
  setCurrentDirectory workingDir

-- | Generate an SRS softifact with a specific solution softifact.
exportSmithEtAlSrsWCode :: System -> SRSDecl -> String -> Choices -> [Mod] -> IO ()
exportSmithEtAlSrsWCode syst srsDecl srsFileName chcs extraModules = do
  exportSmithEtAlSrs syst srsDecl srsFileName
  exportCode syst chcs extraModules

-- | Generate an SRS softifact with a zoo of solution softifacts.
exportSmithEtAlSrsWCodeZoo :: System -> SRSDecl -> String -> [(Choices, [Mod])] -> IO ()
exportSmithEtAlSrsWCodeZoo syst srsDecl srsFileName chcsMods = do
  exportSmithEtAlSrs syst srsDecl srsFileName
  exportCodeZoo syst chcsMods

-- | Generate a JupyterNotebook-based lesson plan.
exportLessonPlan :: System -> LsnDesc -> String -> IO ()
exportLessonPlan syst nbDecl lsnFileName = do
  let nb = mkNb nbDecl S.forT syst
      printSetting = piSys (syst ^. systemdb) (syst ^. refTable) Equational defaultConfiguration
  genDoc (DocSpec (docChoices Lesson []) lsnFileName) nb printSetting

-- | Generate a "website" (HTML file) softifact.
exportWebsite :: System -> Document -> Filename -> IO ()
exportWebsite syst doc fileName = do
  let printSetting = piSys (syst ^. systemdb) (syst ^. refTable) Equational defaultConfiguration
  genDoc (DocSpec (docChoices Website [HTML]) fileName) doc printSetting

-- | Generate a document in one or many flavours (HTML, TeX+Makefile,
-- mdBook+Makefile, or Jupyter Notebook, up to document type).
genDoc :: DocSpec -> Document -> PrintingInformation -> IO ()
genDoc (DocSpec (DC Lesson _) fn) body sm = prntDoc body sm fn Lesson Jupyter
genDoc (DocSpec (DC dt fmts) fn)  body sm = mapM_ (prntDoc body sm fn dt) fmts

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

-- | Common error for when an unsupported SRS format is attempted.
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
writeDoc s _  TeX     _  doc = genTeX doc s
writeDoc s _  HTML    fn doc = genHTML fn $ makeDocument s doc
writeDoc s dt Jupyter _  doc = genJupyter dt $ makeDocument s doc
writeDoc _ _  _       _  _   = srsFormatError

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

-- | Find name of folders created for a "zoo" of executable softifacts.
--
-- FIXME: This is a hack. The generation phase should emit what artifacts it
-- created.
codedDirName :: String -> Choices -> String
codedDirName n Choices {
  architecture = a,
  optFeats = o,
  dataInfo = d,
  maps = m} =
  intercalate "_" [n, codedMod $ modularity a, codedImpTp $ impType a, codedLog $ logging $ logConfig o,
    codedStruct $ inputStructure d, codedConStruct $ constStructure d,
    codedConRepr $ constRepr d, codedSpaceMatch $ spaceMatch m]

codedMod :: Modularity -> String
codedMod Unmodular = "U"
codedMod Modular = "M"

codedImpTp :: ImplementationType -> String
codedImpTp Program = "P"
codedImpTp Library = "L"

codedLog :: [Logging] -> String
codedLog [] = "NoL"
codedLog _ = "L"

codedStruct :: Structure -> String
codedStruct Bundled = "B"
codedStruct Unbundled = "U"

codedConStruct :: ConstantStructure -> String
codedConStruct Inline = "I"
codedConStruct WithInputs = "WI"
codedConStruct (Store s) = codedStruct s

codedConRepr :: ConstantRepr -> String
codedConRepr Var = "V"
codedConRepr Const = "C"

codedSpaceMatch :: SpaceMatch -> String
codedSpaceMatch sm = case sm Real of [Double, Float] -> "D"
                                     [Float, Double] -> "F"
                                     _ -> error
                                       "Unexpected SpaceMatch for Projectile"
