-- | Defines functions to create .toml and .csv config files for mdBook.
module Language.Drasil.Markdown.Config where

import Text.PrettyPrint (Doc, text, vcat, (<+>))
import Data.Map (empty, elems)
import Control.Lens

import Language.Drasil.Markdown.Print (pSpec)
import Language.Drasil.Markdown.Helpers (extractFn)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation(..))
import Database.Drasil (labelledcontentTable)
import Language.Drasil.Printing.Import.Sentence (spec)
import Language.Drasil.Printing.LayoutObj (Filepath)

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

-- | Helper function to render the title
-- 'Sentence' as a 'Doc'
mkTitle :: PrintingInformation -> Sentence -> Doc
mkTitle sm t = text "\"" <> pSpec empty ts <> text "\""
  where 
    ts = spec sm t

-- | Helper function to map the original filepaths of assets
-- to the location mdBook uses.
assetMap :: PrintingInformation -> [(Filepath, FilePath)]
assetMap (PI {_ckdb = cdb}) = 
  [(fp, "src/assets/" ++ extractFn fp) 
  | (LblC { _ctype = Figure _ fp _ }, _) <- elems lct
  ]
  where
    lct = cdb ^. labelledcontentTable
