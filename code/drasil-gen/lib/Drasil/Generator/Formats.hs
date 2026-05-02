-- | Defines output formats for the different documents we can generate.
module Drasil.Generator.Formats (
  -- * Types (Printing Options)
  DocSpec(..), Filename, Format(..),
  -- * Rules
  buildMakefile
) where

import Build.Drasil ((+:+), Command, makeS, mkCheckedCommand, mkCommand, mkFreeVar,
  mkFile, mkRule, mkMakefile, Makefile)
import Drasil.Metadata (watermark)

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension).
type Filename = String

-- | Possible formats for printer output.
data Format = TeX | HTML | Jupyter | MDBook

instance Show Format where
  show TeX     = "PDF"
  show HTML    = "HTML"
  show Jupyter = "Jupyter"
  show MDBook  = "mdBook"

-- | Document specifications. Holds the type of document ('DocType') and its name ('Filename').
data DocSpec = DocSpec Format Filename

-- | Create a 'Makefile' necessary for building a 'DocSpec' when rendered as a
-- concrete artifact. Only relevant to 'TeX' and 'MDBook'.
buildMakefile :: DocSpec -> Maybe Makefile
buildMakefile (DocSpec TeX fn) = Just $ mkMakefile [
  mkRule [watermark] (makeS "srs") [pdfName] [],
  mkFile [] pdfName [makeS $ fn ++ ".tex"] $
    map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
      lualatex, bibtex :: String -> Command
      lualatex = mkCheckedCommand . (+:+) (makeS "lualatex" +:+ mkFreeVar "TEXFLAGS") . makeS
      bibtex = mkCommand . (+:+) (makeS "bibtex" +:+ mkFreeVar "BIBTEXFLAGS") . makeS
      pdfName = makeS $ fn ++ ".pdf"
buildMakefile (DocSpec MDBook _) = Just $ mkMakefile [
  mkRule [watermark] (makeS "build")  [] [build],
  mkRule [] (makeS "server") [] [server]]
  where
    build = mkCheckedCommand $ makeS "mdbook build"
    server = mkCheckedCommand $ makeS "mdbook serve --open"
buildMakefile _ = Nothing
