-- | Common 'Doc'-related functions for writting printers with a little more clarity.
module Utils.Drasil.Document (blank, indent, indentList, contSep, filterEmpty,
  listToDoc, Separator) where

import Text.PrettyPrint.HughesPJ

-- | Separates document sections.
type Separator = Doc

contSep :: Separator
contSep = text "\n"

-- | Creates a blank document with no text.
blank :: Doc
blank = text ""

-- | Indents a document (by 4 spaces).
indent :: Doc -> Doc
indent = nest 4

-- | Indents a list of Docs and combines into one Doc.
indentList :: [Doc] -> Doc
indentList = indent . vcat

-- | Helper for 'makeMd' and 'extLibSec'.
filterEmpty :: [Doc] -> [Doc]
filterEmpty = filter (not . isEmpty) 

-- | Helper for authors and configuration files.
listToDoc :: [String] -> Doc
listToDoc = hsep . punctuate comma . map text
