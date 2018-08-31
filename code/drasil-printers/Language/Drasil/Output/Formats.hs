-- | Defines output formats for the different documents we can generate
module Language.Drasil.Output.Formats where

-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension)
type Filename = String

data DocType = SRS | MG | MIS | Website

data DocSpec = DocSpec DocType Filename

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
