-- | Defines output formats for the different documents we can generate.
module Language.Drasil.Output.Formats where

import Data.Char (toLower)
import Build.Drasil ((+:+), Command, makeS, mkCheckedCommand, mkCommand, mkFreeVar,
  mkFile, mkRule, RuleTransformer(makeRule))
import Language.Drasil.Printers (DocType(..), Format(TeX, MDBook))

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension).
type Filename = String

-- | Document choices include the type of document as well as the file formats we want to generate as.
data DocChoices = DC {
  doctype :: DocType,
  format :: [Format]
}

-- | Document specifications. Holds the type of document ('DocType') and its name ('Filename').
data DocSpec = DocSpec DocChoices Filename

-- | Allows the creation of Makefiles for documents that use LaTeX.
instance RuleTransformer DocSpec where
  makeRule (DocSpec (DC dt [TeX]) fn) = [
    mkRule [] (makeS $ map toLower $ show dt) [pdfName] [],
    mkFile [] pdfName [makeS $ fn ++ ".tex"] $
      map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
        lualatex, bibtex :: String -> Command
        lualatex = mkCheckedCommand . (+:+) (makeS "lualatex" +:+ mkFreeVar "TEXFLAGS") . makeS
        bibtex = mkCommand . (+:+) (makeS "bibtex" +:+ mkFreeVar "BIBTEXFLAGS") . makeS
        pdfName = makeS $ fn ++ ".pdf"
  makeRule (DocSpec (DC _ [MDBook]) _) = [
    mkRule [] (makeS "build")  [] [build],
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
