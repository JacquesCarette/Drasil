-- | Defines output formats for the different documents we can generate
module Language.Drasil.Output.Formats where

import Data.Char (toLower)
import Build.Drasil (Command(C), CommandOpts(IgnoreReturnCode), Rule(R), RuleTransformer(makeRule), Type(Abstract, File))

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension)
type Filename = String

data DocType = SRS | MG | MIS | Website

data DocSpec = DocSpec DocType Filename

lualatex, bibtex :: String -> Command
lualatex = (flip C) [] . (++) "lualatex $(TEXFLAGS) "
bibtex = (flip C) [IgnoreReturnCode] . (++) "bibtex $(BIBTEXFLAGS) "

instance RuleTransformer DocSpec where
  makeRule (DocSpec Website _) = []
  makeRule (DocSpec dt fn) = [
    R (map toLower $ show dt) [fn ++ ".pdf"] Abstract [],
    R (fn ++ ".pdf") [fn ++ ".tex"] File $ map ($ fn) [lualatex, bibtex, lualatex, lualatex]]

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
