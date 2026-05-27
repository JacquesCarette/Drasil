{-# LANGUAGE QuasiQuotes #-}
module Drasil.Generator.SRS (
  -- * Generators
  exportSmithEtAlSrs
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Build.Artifacts (FileLayout, directory, file, localPath, ps, writeFiles)
import Drasil.DocLang (mkGraphInfo)
import Language.Drasil (Stage(Equational), Document(..), checkToC)
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Printers (makeCSS, genHTML, genTeX,
  genMDBook, Notation(Engineering), piSys, PrintingInformation,
  genJupyterSRS)
import Drasil.Makefile ((+:+), makeS, mkCheckedCommand, mkCommand,
  mkFreeVar, mkFile, mkRule, mkMakefile, printMakefile)
import Drasil.Metadata (watermark)
import Drasil.SRSDocument (SRSDecl, mkDoc)
import Language.Drasil.Printing.Import (makeDocument, makeProject)
import Drasil.System (SmithEtAlSRS, refTable, systemdb)
import System.Environment (lookupEnv)

import Drasil.Generator.ChunkDump (dumpEverything)
import Drasil.Generator.Formats (Filename, Format(..))
import Drasil.Generator.SRS.TraceabilityGraphs (outputDot)
import Drasil.Generator.SRS.TypeCheck (typeCheckSI)

-- | Generate an SRS softifact.
exportSmithEtAlSrs :: SmithEtAlSRS -> SRSDecl -> String -> IO ()
exportSmithEtAlSrs syst srsDecl srsFileName = do
  let (srs, syst') = mkDoc syst srsDecl S.forT
      pinfo = piSys (syst' ^. systemdb) (syst' ^. refTable) Equational Engineering
  debugDump syst'
  typeCheckSI syst' -- FIXME: This should be done on `System` creation *or* chunk creation!
  let srsLayout =
        directory [ps|SRS|] $
          map
            ( \x ->
                let x' = show x
                in directory [ps|{x'}|] $
                      prntDoc srs pinfo srsFileName x
            )
            [HTML, TeX, Jupyter, MDBook]
      traceyLayout = outputDot (mkGraphInfo syst') -- FIXME: This *MUST* use syst', NOT syst (or else it misses things!)!
  -- FIXME: Ultimately, there should be a single writeFiles call.
  writeFiles localPath srsLayout
  writeFiles localPath traceyLayout

-- | Internal: Dumps the chunk maps to disk if the `DEBUG_ENV` environment
-- variable is non-empty.
debugDump :: SmithEtAlSRS -> IO ()
debugDump si = do
  -- FIXME: This should be made pure, by passing the 'debug' option instead of using an environment variable
  maybeDebugging <- lookupEnv "DEBUG_ENV"
  case maybeDebugging of
    (Just (_:_)) -> do
      writeFiles localPath $ dumpEverything si
    _ -> mempty

-- | Internal: Creates a `FileLayout` for the SRS in a specific format.
prntDoc :: Document -> PrintingInformation -> String -> Format -> [FileLayout Doc]
prntDoc d pinfo _ MDBook =
  mdBookMakefile : genMDBook (makeProject pinfo d)
prntDoc d pinfo fn Jupyter =
  [file [ps|{fn}.ipynb|] $ genJupyterSRS $ makeDocument pinfo d]
prntDoc d pinfo fn HTML =
  [ file [ps|{fn}.html|] $ genHTML fn $ makeDocument pinfo d,
    file [ps|{fn}.css|] $ makeCSS d
  ]
prntDoc d@(Document _ _ st _) pinfo fn TeX =
  [ file [ps|{fn}.tex|] $ genTeX (makeDocument pinfo $ checkToC d) st pinfo,
    teXMakefile fn
  ]
prntDoc Notebook {} _ _ TeX = error "cannot render notebooks into LaTeX"

-- | Internal: Basic Makefile suitable for building TeX projects.
teXMakefile :: Filename -> FileLayout Doc
teXMakefile fn = file [ps|Makefile|] $ printMakefile $ mkMakefile [
  mkRule [watermark] (makeS "srs") [pdfName] [],
  mkFile [] pdfName [texFile] [lualatex, bibtex, lualatex, lualatex]]
  where
    lualatex = mkCheckedCommand $ makeS "lualatex" +:+ mkFreeVar "TEXFLAGS"    +:+ makeS fn
    bibtex   = mkCommand        $ makeS "bibtex"   +:+ mkFreeVar "BIBTEXFLAGS" +:+ makeS fn
    pdfName  = makeS $ fn ++ ".pdf"
    texFile  = makeS $ fn ++ ".tex"

-- | Internal: Basic Makefile suitable for building mdBook projects.
mdBookMakefile :: FileLayout Doc
mdBookMakefile = file [ps|Makefile|] $ printMakefile $ mkMakefile [
  mkRule [watermark] (makeS "build")  [] [mkCheckedCommand $ makeS "mdbook build"],
  mkRule []          (makeS "server") [] [mkCheckedCommand $ makeS "mdbook serve --open"]]
