-- | Common 'Doc'-related functions for writting printers with a little more clarity.
module Utils.Drasil.Document (blank, indent, (+:+.), indentList, drasilImage, contSep) where

import Text.PrettyPrint.HughesPJ (Doc, nest, text, vcat)

-- | Creates a blank document with no text.
blank :: Doc
blank = text ""

-- | Indents a document (by 4 spaces).
indent :: Doc -> Doc
indent = nest 4

-- | Helper which concatenates two Docs and appends a period.
(+:+.) :: Doc -> Doc -> Doc
a +:+. b = a <> b <> text "."

-- | Indents a list of Docs and combines into one Doc.
indentList :: [Doc] -> Doc
indentList = vcat . map indent  -- TODO: Isn't this just `indent . vcat`? This would be a bit more efficient too

-- Function to create the prefix for the path of the Drasil Logo
buildPath :: Int -> String
buildPath num = filter (/= ' ') $ unwords $ replicate num "../"

-- | Drasil Tree icon. Uses HTML directly to format image since normal markdown doesn't support it.
drasilImage :: Int -> Doc
drasilImage num = alignImage (buildPath num ++
  "drasil-website/WebInfo/images/Icon.png")

-- | Aligns an image to the center using HTML, since markdown doesn't support it.
alignImage :: FilePath -> Doc
alignImage img = text "<p align=\"center\">" <> contSep <> text ("<img src=\"" 
  ++ img ++ "\" alt=\"Drasil Tree\" width=\"200\" />") <> contSep <> text "</p>"

-- | Separates document sections.
type Separator = Doc

contSep :: Separator
contSep = text "\n"
