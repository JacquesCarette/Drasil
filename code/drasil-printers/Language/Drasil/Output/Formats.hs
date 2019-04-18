-- | Defines output formats for the different documents we can generate
module Language.Drasil.Output.Formats where

import Build.Drasil (RuleTransformer(makeRule), Type(Phony, TeX))

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension)
type Filename = String

data DocType = SRS | MG | MIS | Website

data DocSpec = DocSpec DocType Filename

instance RuleTransformer DocSpec where
  makeRule (DocSpec SRS fn)     = [(Phony, "srs", [fn ++ ".pdf"]), (TeX, fn, [])]
  makeRule (DocSpec MG  fn)     = [(Phony, "mg" , [fn ++ ".pdf"]), (TeX, fn, [])]
  makeRule (DocSpec MIS fn)     = [(Phony, "mis", [fn ++ ".pdf"]), (TeX, fn, [])]
  makeRule (DocSpec Website _)  = []

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
