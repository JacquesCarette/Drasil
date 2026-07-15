{-# LANGUAGE OverloadedStrings #-}
-- | Helper functions for creating HTML printers (specifically, HTML tag wrappers).
module Language.Drasil.HTML2.Helpers (
  -- * Types
  BibFormatter(..),
  articleTitle, author,
  foldRaw
) where

import Drasil.Data.Formats.HTML
import Language.Drasil.Printing.AST (Spec)

-- | Data type that carries functions that vary
-- for bib printing
data BibFormatter = BibFormatter {
  -- | Emphasis (italics) rendering
  emph :: [HTMLBody] -> [HTMLBody],
  -- | Spec rendering
  spec :: Spec -> [HTMLBody]
}

articleTitle, author :: [HTMLBody] -> HTMLBody
articleTitle t = Div [Attr "class" "title"]  [Heading H1 [] t]
author       a = Div [Attr "class" "author"] [Heading H2 [] a]

foldRaw :: [HTMLBody] -> [HTMLBody]
foldRaw [] = []
foldRaw (RawText a : RawText b : rest) = foldRaw (RawText (a <> b) : rest)
foldRaw (x : xs) = x : foldRaw xs
