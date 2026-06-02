{-# LANGUAGE QuasiQuotes #-}
module Drasil.Generator.SRS (
  -- * SRS Generator
  genSmithEtAlSrs
) where

import Prelude hiding (id)
import Control.Lens ((^.))

import Drasil.DocLang (mkGraphInfo)
import Drasil.FileHandling (FileLayout, directory, file, ps)
import Language.Drasil (Stage(Equational), Document(..), checkToC)
import Language.Drasil.Printers (makeCSS, genHTML, genTeX,
  genMDBook, Notation(Engineering), piSys, PrintingInformation,
  genJupyterSRS)
import Drasil.Makefile ((+:+), makeS, mkCheckedCommand, mkCommand,
  mkFreeVar, mkFile, mkRule, mkMakefile, printMakefile)
import Drasil.Metadata (watermark)
import Language.Drasil.Printing.Import (makeDocument, makeProject)
import Drasil.System (SmithEtAlSRS, refTable, systemdb)

import Drasil.Generator.Formats (Filename, Format(..))
import Drasil.Generator.SRS.TraceabilityGraphs (outputDot)

-- | Generate Drasil's SRS (in HTML, TeX, Jupyter, and MDBook formats).
genSmithEtAlSrs :: SmithEtAlSRS -> Document -> String -> [FileLayout]
genSmithEtAlSrs syst doc srsFileName =
  [ srsLayout,
    traceyLayout
  ]
  where
    pinfo = piSys (syst ^. systemdb) (syst ^. refTable) Equational Engineering
    srsLayout =
      directory [ps|SRS|] $
        map
          ( \x ->
              let x' = show x
              in directory [ps|{x'}|] $
                    prntDoc doc pinfo srsFileName x
          )
          [HTML, TeX, Jupyter, MDBook]
    traceyLayout = outputDot (mkGraphInfo syst)

-- | Internal: Render an SRS in a specified 'Format' and lay out artifacts into
-- a `[FileLayout]`.
prntDoc :: Document -> PrintingInformation -> String -> Format -> [FileLayout]
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
teXMakefile :: Filename -> FileLayout
teXMakefile fn = file [ps|Makefile|] $ printMakefile $ mkMakefile [
  mkRule [watermark] (makeS "srs") [pdfName] [],
  mkFile [] pdfName [texFile] [lualatex, bibtex, lualatex, lualatex]]
  where
    lualatex = mkCheckedCommand $ makeS "lualatex" +:+ mkFreeVar "TEXFLAGS"    +:+ makeS fn
    bibtex   = mkCommand        $ makeS "bibtex"   +:+ mkFreeVar "BIBTEXFLAGS" +:+ makeS fn
    pdfName  = makeS $ fn ++ ".pdf"
    texFile  = makeS $ fn ++ ".tex"

-- | Internal: Basic Makefile suitable for building mdBook projects.
mdBookMakefile :: FileLayout
mdBookMakefile = file [ps|Makefile|] $ printMakefile $ mkMakefile [
  mkRule [watermark] (makeS "build")  [] [mkCheckedCommand $ makeS "mdbook build"],
  mkRule []          (makeS "server") [] [mkCheckedCommand $ makeS "mdbook serve --open"]]
