-- | Defines output formats for the different documents we can generate
module Language.Drasil.Output.Formats where

import Data.Char (toLower)
import Build.Drasil (Command, mkCheckedCommand, mkCommand, mkFile, mkRule, RuleTransformer(makeRule))

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension)
type Filename = String

data DocType = SRS | MG | MIS | Website

data DocSpec = DocSpec DocType Filename

instance RuleTransformer DocSpec where
  makeRule (DocSpec Website _) = []
  makeRule (DocSpec dt fn) = [
    mkRule (map toLower $ show dt) [fn ++ ".pdf"] [],
    mkFile (fn ++ ".pdf") [fn ++ ".tex"] $
      map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
        lualatex, bibtex :: String -> Command
        lualatex = mkCheckedCommand . (++) "lualatex $(TEXFLAGS) "
        bibtex = mkCommand . (++) "bibtex $(BIBTEXFLAGS) "

instance Show DocType where
  show SRS      = "SRS"
  show MG       = "MG"
  show MIS      = "MIS"
  show Website  = "Website"
             
-- | LaTeX helper
data DocClass = DocClass (Maybe String) String
-- | LaTeX helper for adding packages
newtype UsePackages = UsePackages [String] -- Package name list
-- | LaTeX helper
data ExDoc = ExDoc (Maybe String) String
