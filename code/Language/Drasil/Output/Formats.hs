-- | Defines output formats for the different documents we can generate
module Language.Drasil.Output.Formats where

type Filename = String

-- | When choosing your document type, you must specify the filename for
-- the generated output (specified /without/ a file extension)
data DocType = SRS Filename     --Filename with no extension
             | MG Filename
             | MIS Filename
             | Website Filename

instance Show DocType where
  show (SRS _) = "SRS"
  show (MG _)  = "MG"
  show (MIS _) = "MIS"
  show (Website _) = "Website"
             
-- | LaTeX helper
data DocClass = DocClass (Maybe String) String
-- | LaTeX helper for adding packages
newtype UsePackages = UsePackages [String] -- Package name list
-- | LaTeX helper
data ExDoc = ExDoc (Maybe String) String
