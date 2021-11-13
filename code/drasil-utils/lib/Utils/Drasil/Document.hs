-- | Common 'Doc'-related functions for writting printers with a little more clarity.
module Utils.Drasil.Document (blank, indent, indentList) where

import Text.PrettyPrint.HughesPJ (Doc, nest, text, vcat)

-- | Creates a blank document with no text.
blank :: Doc
blank = text ""

-- | Indents a document (by 4 spaces).
indent :: Doc -> Doc
indent = nest 4

-- | Indents a list of Docs and combines into one Doc.
indentList :: [Doc] -> Doc
indentList = vcat . map indent  -- TODO: Isn't this just `indent . vcat`? This would be a bit more efficient too
