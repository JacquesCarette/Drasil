module Utils.Drasil.Document (blank, indent, indentList) where

import Text.PrettyPrint.HughesPJ (Doc, nest, text, vcat)

blank :: Doc
blank = text ""

indent :: Doc -> Doc
indent = nest 4

indentList :: [Doc] -> Doc
indentList = vcat . map indent