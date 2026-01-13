-- | Defines functions to create .toml and .csv config files for mdBook.
module Language.Drasil.Markdown.Config (makeBook, makeRequirements, mkTitle) where

import Data.List (intercalate)
import Control.Lens((^.))
import Text.PrettyPrint (Doc, text, vcat, (<+>))
import System.FilePath (takeFileName)

import Language.Drasil (Document(Document), LabelledContent(LblC, _ctype),
  RawContent(Figure), Sentence)

import Drasil.Database.SearchTools (findAllLabelledContent)
import Language.Drasil.Markdown.Print (pSpec)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, sysdb)
import Language.Drasil.Printing.Import.Sentence (spec)
import Language.Drasil.Printing.LayoutObj (Filepath)

-- | Prints the .toml config file for mdBook.
makeBook :: Document -> PrintingInformation -> Doc
makeBook (Document t _ _ _) sm = vcat [
  text "[book]",
  text "language = \"en\"",
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
    | LblC { _ctype = Figure _ fp _ _ } <- findAllLabelledContent (pinfo ^. sysdb)]

-- | Creates a CSV file as a 'Doc' from a 'String' matrix.
makeCSV :: [[String]] -> Doc
makeCSV rows = vcat $ map (text . formatRow) rows
  where
    -- | Seperates each row item with a comma.
    formatRow :: [String] -> String
    formatRow = intercalate "," . map escape

    -- | Adds quotations around the item if it contains ',', '"', \n', or ' '.
    escape :: String -> String
    escape s
      | any (`elem` ",\"\n ") s = "\"" ++ concatMap escapeChar s ++ "\""
      | otherwise = s

    -- | Escapes double quotes.
    escapeChar :: Char -> String
    escapeChar '"' = "\"\""
    escapeChar c   = [c]
