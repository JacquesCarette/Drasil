-- | Defines output formats for the different documents we can generate.
module Language.Drasil.Output.Formats where

import Data.Char (toLower)
import Build.Drasil ((+:+), Command, makeS, mkCheckedCommand, mkCommand, mkFreeVar,
  mkFile, mkRule, RuleTransformer(makeRule))

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension).
type Filename = String

-- | Document types include Software Requirements Specification, MG, MIS, and Website.
-- The first three will generate as TeX files while the Website generates as HTML.
data DocType = SRS | MG | MIS | Website

-- | Document specifications. Holds the type of document ('DocType') and its name ('Filename').
data DocSpec = DocSpec DocType Filename

-- | Allows the creation of Makefiles for documents that use LaTeX.
instance RuleTransformer DocSpec where
  makeRule (DocSpec Website _) = []
  makeRule (DocSpec dt fn) = [
    mkRule (makeS $ map toLower $ show dt) [pdfName] [],
    mkFile pdfName [makeS $ fn ++ ".tex"] $
      map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
        lualatex, bibtex :: String -> Command
        lualatex = mkCheckedCommand . (+:+) (makeS "lualatex" +:+ mkFreeVar "TEXFLAGS") . makeS
        bibtex = mkCommand . (+:+) (makeS "bibtex" +:+ mkFreeVar "BIBTEXFLAGS") . makeS
        pdfName = makeS $ fn ++ ".pdf"

-- | Shows the different types of documents.
instance Show DocType where
  show SRS      = "SRS"
  show MG       = "MG"
  show MIS      = "MIS"
  show Website  = "Website"
             
-- | LaTeX helper.
data DocClass = DocClass (Maybe String) String
-- | LaTeX helper for adding packages. Wraps a list of package names.
newtype UsePackages = UsePackages [String] -- Package name list
-- | LaTeX helper.
data ExDoc = ExDoc (Maybe String) String
