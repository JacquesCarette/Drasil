module Utils.Drasil.Document (indent, indentList) where

import Text.PrettyPrint.HughesPJ (Doc, nest, vcat)

indent :: Doc -> Doc
indent = nest 4

indentList :: [Doc] -> Doc
indentList = vcat . map indent