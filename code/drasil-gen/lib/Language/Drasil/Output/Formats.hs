-- | Defines output formats for the different documents we can generate.
module Language.Drasil.Output.Formats where

import Data.Char (toLower)
import System.FilePath (takeDirectory)
import Build.Drasil ((+:+), Command, makeS, mkCheckedCommand, mkCommand, mkFreeVar,
  mkFile, mkRule, RuleTransformer(makeRule))
import Language.Drasil.Printers (DocType(..), Format(TeX, MDBook),
  PrintingInformation, assetMap)

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

-- | Makefile specification.
-- Holds the type of document ('DocType'), document format ('Format'),
-- its name ('Filename'), and printing information ('PrintingInformation')
data MakeSpec = MakeSpec DocType Format Filename PrintingInformation

-- | Allows the creation of Makefiles for documents that use LaTeX and/or mdBook.
instance RuleTransformer MakeSpec where
  -- Makefile for TeX to compile the .tex files to PDF.
  makeRule (MakeSpec dt TeX fn _) = [
    mkRule [] (makeS $ map toLower $ show dt) [pdfName] [],
    mkFile [] pdfName [makeS $ fn ++ ".tex"] $
      map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
        lualatex, bibtex :: String -> Command
        lualatex = mkCheckedCommand . (+:+) (makeS "lualatex" +:+ mkFreeVar "TEXFLAGS") . makeS
        bibtex = mkCommand . (+:+) (makeS "bibtex" +:+ mkFreeVar "BIBTEXFLAGS") . makeS
        pdfName = makeS $ fn ++ ".pdf"
  -- Makefile for MDBook to build book, launch server, and copy assets.
  makeRule (MakeSpec _ MDBook _ sm) = [
    mkRule [] (makeS "build") [assets] [build],
    mkRule [] (makeS "server") [assets] [server],
    mkRule [] (assets) [] (map cmd (assetMap sm))]
    where
      build = mkCheckedCommand $ makeS "mdbook build"
      server = mkCheckedCommand $ makeS "mdbook serve --open"
      assets = makeS "assets" 
      cmd st = mkCommand $ makeS $ mkdir st ++ " && " ++ copy st 
      copy (s, t) = "cp \"" ++ s ++ "\" \"" ++ t ++ "\""
      mkdir (_, t) = "mkdir -p \"" ++ takeDirectory t ++ "\""
  makeRule _ = []

-- | LaTeX helper.
data DocClass = DocClass (Maybe String) String
-- | LaTeX helper for adding packages. Wraps a list of package names.
newtype UsePackages = UsePackages [String] -- Package name list
-- | LaTeX helper.
data ExDoc = ExDoc (Maybe String) String
