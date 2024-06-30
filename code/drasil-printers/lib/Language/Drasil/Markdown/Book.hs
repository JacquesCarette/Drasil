-- | Defines functions to create .toml config files for mdBook.
module Language.Drasil.Markdown.Book where

import Text.PrettyPrint (Doc, text, vcat, (<+>))
import Data.Map (empty)

import Language.Drasil.Markdown.Print (pSpec)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)
import Language.Drasil.Printing.Import.Sentence (spec)

import Language.Drasil hiding (Expr)

-- | Generates the .toml config file for mdBook.
makeBook :: Document -> PrintingInformation -> Doc  
makeBook (Document t _ _ _) sm = vcat [
  text "[book]",
  text "language = \"en\"",
  text "multilingual = false",
  text "src = \"src\"",
  text "title =" <+> mkTitle sm t,
  text "[output.html]",
  text "smart-punctuation = true",
  text "mathjax-support = true"
  ]
makeBook _ _ = error "Type not supported: Notebook."

mkTitle :: PrintingInformation -> Sentence -> Doc
mkTitle sm t = text "\"" <> pSpec empty ts <> text "\""
  where 
    ts = spec sm t
