-- | Defines functions to create .toml and .csv config files for mdBook.
module Language.Drasil.Markdown.Config where

import Control.Lens((^.))
import Text.PrettyPrint (Doc, text, vcat, (<+>))
import System.FilePath (takeFileName)

import Drasil.Database.SearchTools (findAllLabelledContent)
import Language.Drasil (Document(Document), LabelledContent(LblC, _ctype),
  RawContent(Figure), Sentence)
import Language.Drasil.Markdown.Print (pSpec)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, ckdb)
import Language.Drasil.Printing.Import.Sentence (spec)
import Language.Drasil.Printing.LayoutObj (Filepath)
import Utils.Drasil (makeCSV)

-- | Prints the .toml config file for mdBook.
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

-- | Prints the .csv file mapping the original filepaths of assets to the
-- location mdBook uses.
makeRequirements :: PrintingInformation -> Doc
makeRequirements sm = makeCSV $ ["Original", "Copy"] : assetMat sm

-- | Render a title 'Sentence'.
mkTitle :: PrintingInformation -> Sentence -> Doc
mkTitle sm t = text "\"" <> pSpec mempty (spec sm t) <> text "\""

-- | Map the original filepaths of assets to the location mdBook generator
-- needs.
assetMat :: PrintingInformation -> [[Filepath]]
assetMat pinfo = 
  [[fp, "src/assets/" ++ takeFileName fp]
    | LblC { _ctype = Figure _ fp _ _ } <- findAllLabelledContent (pinfo ^. ckdb)]
