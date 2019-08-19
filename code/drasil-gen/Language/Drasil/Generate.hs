module Language.Drasil.Generate (gen, genCode) where

import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, render)
import Prelude hiding (id)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory,
  setCurrentDirectory)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian)

import Build.Drasil (genMake)
import Language.Drasil
import Language.Drasil.Printers (Format(TeX, HTML), DocSpec(DocSpec), 
  DocType(SRS, MG, MIS, Website), Filename, makeCSS, genHTML,
  genTeX, PrintingInformation)
import Language.Drasil.Code (generator, generateCode, Choices(..), CodeSpec(..),
  CodeSystInfo(..), Lang(..), readWithDataDesc, sampleInputDD, unJC, unPC, 
  unCSC, unCPPC)

-- | Generate a number of artifacts based on a list of recipes.
gen :: DocSpec -> Document -> PrintingInformation -> IO ()
gen ds fn sm = prnt sm ds fn

-- | Generate the output artifacts (TeX+Makefile or HTML)
prnt :: PrintingInformation -> DocSpec -> Document -> IO ()
prnt sm dt@(DocSpec Website fn) body =
  do prntDoc dt body sm
     outh2 <- openFile ("Website/" ++ fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2
prnt sm dt@(DocSpec _ _) body =
  do prntDoc dt body sm
     prntMake dt

-- | Helper for writing the documents (TeX / HTML) to file
prntDoc :: DocSpec -> Document -> PrintingInformation -> IO ()
prntDoc (DocSpec dt fn) = prntDoc' dt fn (fmt dt)
  where fmt SRS = TeX
        fmt MG  = TeX
        fmt MIS = TeX
        fmt Website = HTML

prntDoc' :: Show a => a -> String -> Format -> Document -> PrintingInformation -> IO ()
prntDoc' dt' fn format body' sm = do
  createDirectoryIfMissing False $ show dt'
  outh <- openFile (show dt' ++ "/" ++ fn ++ getExt format) WriteMode
  hPutStrLn outh $ render $ writeDoc sm format fn body'
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
writeDoc :: PrintingInformation -> Format -> Filename -> Document -> Doc
writeDoc s TeX  _  doc = genTeX doc s
writeDoc s HTML fn doc = genHTML s fn doc
writeDoc _    _  _   _ = error "we can only write TeX/HTML (for now)"

-- | Calls the code generator
genCode :: Choices -> CodeSpec -> IO ()
genCode chs spec = do 
  workingDir <- getCurrentDirectory
  time <- getCurrentTime
  sampData <- readWithDataDesc (smplData $ csi spec) $ sampleInputDD (extInputs $ csi spec)
  createDirectoryIfMissing False "src"
  setCurrentDirectory "src"
  let genLangCode Java = genCall Java unJC
      genLangCode Python = genCall Python unPC
      genLangCode CSharp = genCall CSharp unCSC
      genLangCode Cpp = genCall Cpp unCPPC
      genCall lng unRepr = generateCode lng unRepr $ generator (showGregorian 
        $ utctDay time) sampData chs spec
  mapM_ genLangCode (lang chs)
  setCurrentDirectory workingDir
