-- | Defines functions to create .toml and .csv config files for mdBook.
module Language.Drasil.Markdown.Config where

import Text.PrettyPrint (Doc, text, vcat, (<+>))
import Data.Map (empty, elems)
import Control.Lens
import System.FilePath (takeFileName)

import Utils.Drasil (makeCSV)
import Language.Drasil.Markdown.Print (pSpec)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation(..))
import Database.Drasil (labelledcontentTable)
import Language.Drasil.Printing.Import.Sentence (spec)
import Language.Drasil.Printing.LayoutObj (Filepath)

import Language.Drasil hiding (Expr)

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

-- | Prints the .csv file mapping the original 
-- filepaths of assets to the location mdBook uses.
makeRequirements :: PrintingInformation -> Doc
makeRequirements sm = makeCSV $ ["Original", "Copy"] : assetMat sm

-- | Helper function to render the title
-- 'Sentence' as a 'Doc'
mkTitle :: PrintingInformation -> Sentence -> Doc
mkTitle sm t = text "\"" <> pSpec empty (spec sm t) <> text "\""

-- | Helper function to map the original filepaths of assets
-- to the location mdBook uses.
assetMat :: PrintingInformation -> [[Filepath]]
assetMat (PI {_ckdb = cdb}) = 
  [[fp, "src/assets/" ++ takeFileName fp] 
  | (LblC { _ctype = Figure _ fp _ }, _) <- elems $ cdb ^. labelledcontentTable
  ]
