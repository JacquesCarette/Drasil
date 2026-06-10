-- | Defines output formats for the different documents we can generate.
module Drasil.Generator.Formats (
  -- * Types (Printing Options)
  Filename, Format(..)
) where

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
