-- | Defines output formats for the different documents we can generate.
module Drasil.Generator.Formats (
  -- * Types (Printing Options)
  DocType(..), DocSpec(DocSpec), DocChoices(..),
  DocClass(..), UsePackages(..), ExDoc(..), Filename,
  Format(..),
  -- * Constructors
  docChoices
) where

import Data.Char (toLower)

import Build.Drasil ((+:+), Command, makeS, mkCheckedCommand, mkCommand, mkFreeVar,
  mkFile, mkRule, RuleTransformer(makeRule))
import Drasil.Metadata (watermark)

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension).
type Filename = String

-- | Document types include Software Requirements Specification and Website.
-- Choosing SRS will generate both TeX and HTML files, while Website generates
-- only as HTML. This also determines what folders the generated files will be
-- placed into.
data DocType = SRS | Website

-- | Possible formats for printer output.
data Format = TeX | Plain | HTML | Jupyter | MDBook

instance Show Format where
  show TeX     = "PDF"
  show Plain   = "Plain"
  show HTML    = "HTML"
  show Jupyter = "Jupyter"
  show MDBook  = "mdBook"

-- | Shows the different types of documents.
instance Show DocType where
  show SRS     = "SRS"
  show Website = "Website"

-- | Document choices include the type of document as well as the file formats we want to generate as.
data DocChoices = DC {
  doctype :: DocType,
  format :: [Format]
}

-- | Document specifications. Holds the type of document ('DocType') and its name ('Filename').
data DocSpec = DocSpec DocChoices Filename

-- | Constructor for users to choose their document options
docChoices :: DocType -> [Format] -> DocChoices
docChoices = DC

-- | Allows the creation of Makefiles for documents that use LaTeX.
instance RuleTransformer DocSpec where
  makeRule (DocSpec (DC dt [TeX]) fn) = [
    mkRule [watermark] (makeS $ map toLower $ show dt) [pdfName] [],
    mkFile [] pdfName [makeS $ fn ++ ".tex"] $
      map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
        lualatex, bibtex :: String -> Command
        lualatex = mkCheckedCommand . (+:+) (makeS "lualatex" +:+ mkFreeVar "TEXFLAGS") . makeS
        bibtex = mkCommand . (+:+) (makeS "bibtex" +:+ mkFreeVar "BIBTEXFLAGS") . makeS
        pdfName = makeS $ fn ++ ".pdf"
  makeRule (DocSpec (DC _ [MDBook]) _) = [
    mkRule [watermark] (makeS "build")  [] [build],
    mkRule [] (makeS "server") [] [server]]
    where
      build = mkCheckedCommand $ makeS "mdbook build"
      server = mkCheckedCommand $ makeS "mdbook serve --open"
  makeRule _ = []

-- | LaTeX helper.
data DocClass = DocClass (Maybe String) String

-- | LaTeX helper for adding packages. Wraps a list of package names.
newtype UsePackages = UsePackages [String] -- Package name list

-- | LaTeX helper.
data ExDoc = ExDoc (Maybe String) String
