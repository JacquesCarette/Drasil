{-# LANGUAGE QuasiQuotes #-}
module Drasil.Generator.SRS (
  -- * SRS Generator
  genSmithEtAlSrs
) where

import Prelude hiding (id)
import Control.Lens ((^.))

import Drasil.FileHandling (FileLayout, directory, file, ps)
import Language.Drasil (Stage(Equational))
import Language.Drasil.Document (Document(..), ShowTableOfContents (ToC))
import Language.Drasil.Printers (genericCSS, genHTML, genTeX,
  genMDBook, Notation(Engineering), piSys, PrintingInformation,
  genJupyterSRS, makeDocument, makeProject)
import Drasil.Makefile ((+:+), makeS, mkCheckedCommand, mkCommand,
  mkFreeVar, mkFile, mkRule, mkMakefile, printMakefile)
import Drasil.Metadata (watermark)
import Drasil.SRS (mkGraphInfo, SmithEtAlSRS)
import Drasil.System (systemdb)

import Drasil.Generator.Formats (Filename, Format(..))
import Drasil.Generator.SRS.TraceabilityGraphs (outputDot)

-- | Generate Drasil's SRS (in HTML, TeX, Jupyter, and MDBook formats).
genSmithEtAlSrs :: SmithEtAlSRS -> Document -> String -> [FileLayout]
genSmithEtAlSrs syst doc srsFileName =
  [ srsLayout,
    traceyLayout
  ]
  where
    pinfo = piSys (syst ^. systemdb) Equational Engineering
    srsLayout =
      directory [ps|SRS|] $
        map
          ( \x ->
              let x' = show x
              in directory [ps|{x'}|] $
                    prntDoc doc  pinfo ToC srsFileName x
          )
          [HTML, TeX, Jupyter, MDBook]
    traceyLayout = outputDot (mkGraphInfo syst)

-- | Internal: Render an SRS in a specified 'Format' and lay out artifacts into
-- a `[FileLayout]`.
prntDoc :: Document -> PrintingInformation -> ShowTableOfContents -> String -> Format -> [FileLayout]
prntDoc d pinfo _ _ MDBook =
  mdBookMakefile : genMDBook (makeProject pinfo d)
prntDoc d pinfo _ fn Jupyter =
  [file [ps|{fn}.ipynb|] $ genJupyterSRS $ makeDocument pinfo d]
prntDoc d pinfo _ fn HTML =
  [ file [ps|{fn}.html|] $ genHTML fn $ makeDocument pinfo d,
    file [ps|{fn}.css|] genericCSS
  ]
prntDoc Notebook{} _ _ _ TeX = error "cannot render notebooks into LaTeX"
prntDoc d pinfo st fn TeX =
  [ file [ps|{fn}.tex|] $ genTeX (makeDocument pinfo d) st pinfo,
    teXMakefile fn
  ]

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
