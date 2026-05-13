{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.SRS (
  -- * Generators
  exportSmithEtAlSrs
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Build.Artifacts (FileLayout, directory, file, localPath, ps, writeFiles)
import Build.Drasil (printMakefile)
import Drasil.DocLang (mkGraphInfo)
import Language.Drasil (Stage(Equational), Document(Document, Notebook),
  ShowTableOfContents, checkToC)
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Printers (makeCSS, makeRequirements, genHTML, genTeX,
  genMDBook, makeBook, Notation(Engineering), piSys, PrintingInformation,
  genJupyterSRS)
import Data.Maybe (maybeToList)
import Drasil.SRSDocument (SRSDecl, mkDoc)
import Language.Drasil.Printing.Import (makeDocument, makeProject)
import Drasil.System (SmithEtAlSRS, refTable, systemdb, lbldCntnt)

import Drasil.Generator.ChunkDump (dumpEverything)
import Drasil.Generator.Formats (DocSpec(..), Filename, Format(..), buildMakefile)
import Drasil.Generator.SRS.TraceabilityGraphs (outputDot)
import Drasil.Generator.SRS.TypeCheck (typeCheckSI)

-- | Generate an SRS softifact.
exportSmithEtAlSrs :: SmithEtAlSRS -> SRSDecl -> String -> IO ()
exportSmithEtAlSrs syst srsDecl srsFileName = do
  let (srs, syst') = mkDoc syst srsDecl S.forT
      printfo = piSys (syst' ^. systemdb) (syst' ^. refTable) Equational Engineering (syst' ^. lbldCntnt)
  dumpEverything syst' ".drasil/"
  typeCheckSI syst' -- FIXME: This should be done on `System` creation *or* chunk creation!
  let srsLayout    = directory [ps|SRS|] $
                       map (prntDoc srs printfo srsFileName)
                       [HTML, TeX, Jupyter, MDBook]
      traceyLayout = outputDot (mkGraphInfo syst') -- FIXME: This *MUST* use syst', NOT syst (or else it misses things!)!
  -- FIXME: Ultimately, there should be a single writeFiles call.
  writeFiles localPath srsLayout
  writeFiles localPath traceyLayout

-- | Internal: Creates a `FileLayout` for the SRS in a specific format.
prntDoc :: Document -> PrintingInformation -> String -> Format -> FileLayout Doc
prntDoc d pinfo fn fmt =
    directory (outPath fmt) $ (doc : mMakefile) ++ extraFiles fmt
  where
    doc = prntDoc' fn fmt d pinfo
    mMakefile = maybeToList $ prntMake $ DocSpec fmt fn

    outPath HTML    = [ps|HTML|]
    outPath TeX     = [ps|PDF|]
    outPath Jupyter = [ps|Jupyter|]
    outPath MDBook  = [ps|mdBook|]

    extraFiles HTML   = [ file [ps|{fn}.css|] $ makeCSS d ]
    extraFiles MDBook = [ file [ps|book.toml|] $ makeBook d pinfo,
                          file [ps|.drasil-requirements.csv|] $ makeRequirements pinfo
                        ]
    extraFiles _      = []

-- | Internal: Common error for when an unsupported SRS format is attempted.
srsFormatError :: a
srsFormatError = error "We can only write TeX/HTML/JSON/MDBook (for now)."

-- | Internal: Creates a `FileLayout` for the core content of an SRS in a
-- specific format.
prntDoc' :: String -> Format -> Document -> PrintingInformation -> FileLayout Doc
prntDoc' _ MDBook body' sm =
  directory [ps|src|] $ map writeDocToFile con
  where
    con = writeDoc' sm MDBook body'
    writeDocToFile (fp, d) = file [ps|{fp}.md|] d
prntDoc' fn format body' sm =
  file [ps|{fn}.{ext}|] $ writeDoc sm format fn body'
  where
    ext = getExt format
    -- | Gets extension for a particular format.
    -- MDBook case is handled above.
    getExt  TeX         = "tex"
    getExt  HTML        = "html"
    getExt  Jupyter     = "ipynb"
    getExt _            = srsFormatError

-- | Internal: Creates a `FileLayout` for the Makefile of an SRS,
-- if applicable to the secified Format.
prntMake :: DocSpec -> Maybe (FileLayout Doc)
prntMake = fmap (file [ps|Makefile|] . printMakefile) . buildMakefile

-- | Internal: Renders single-page documents.
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

-- | Internal: Renders multi-page documents.
writeDoc' :: PrintingInformation -> Format -> Document -> [(Filename, Doc)]
writeDoc' s MDBook doc = genMDBook $ makeProject s doc
writeDoc' _ _      _   = srsFormatError
