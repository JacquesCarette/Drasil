-- | Defines functions to create .toml and .csv config files for mdBook.
module Language.Drasil.Markdown.Config where

import Text.PrettyPrint (Doc, text, vcat, (<+>))
import Data.Map (empty, elems)
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

-- | Prints the .csv file mapping the original filepaths of assets to the
-- location mdBook uses.
makeRequirements :: PrintingInformation -> Doc
makeRequirements sm = makeCSV $ ["Original", "Copy"] : assetMat sm

-- | Render a title 'Sentence'.
mkTitle :: PrintingInformation -> Sentence -> Doc
mkTitle sm t = text "\"" <> pSpec empty (spec sm t) <> text "\""

-- | Map the original filepaths of assets to the location mdBook generator
-- needs.
assetMat :: PrintingInformation -> [[Filepath]]
assetMat (PI {_ckdb = cdb}) = 
  [[fp, "src/assets/" ++ takeFileName fp] 
  | (LblC { _ctype = Figure _ fp _ _ }, _) <- elems $ labelledcontentTable cdb
  ]
  -- FIXME: HACK: Almost nothing should ever be "gathering everything" from a
  -- ChunkDB unless it is intended to do something highly generic, such as an
  -- analysis, like walking along a UID-tree to build the "trace graphs." We
  -- should not be using this as part of the renderer, however, to search for
  -- all $x$ for rendering. The ChunkDB is allowed to have more knowledge in it
  -- than we strictly need for a specific use of Drasil. To get the point
  -- across, we should be able to merge ALL of our individual ChunkDBs in
  -- Drasil, feed the same ChunkDBs into the SmithEtAl generator along with the
  -- /specific/ problem descriptions and generate the exact same artifacts. The
  -- fact that there would be knowledge/chunks in the ChunkDB that the generator
  -- would never access should have no impact.
