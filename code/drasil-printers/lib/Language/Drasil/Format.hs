-- | Possible formats for printer output.
module Language.Drasil.Format where

-- | Document types include Software Requirements Specification and Website.
-- Choosing SRS will generate both TeX and HTML files, while Website generates only as HTML.
-- This also determines what folders the generated files will be placed into.
data DocType = SRS | Website | Jupyter

-- | Possible formats for printer output.
data Format = TeX | Plain | HTML | JSON | Markdown MDFlavour | MDBook

-- | Possible flavours of Markdown.
data MDFlavour = GitHub

-- | Shows the different types of documents.
instance Show DocType where
  show Jupyter  = "Jupyter"
  show SRS      = "SRS"
  show Website  = "Website"