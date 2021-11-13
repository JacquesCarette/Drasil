-- | Gather Drasil's utility functions and re-export for easy use.
-- For now, does not include combinators (Sentence.hs, NounPhrase.hs, Concepts.hs)
module Utils.Drasil (
  -- * Documents
  -- | From "Utils.Drasil.Document".
  blank, indent, indentList,

  -- * Language
  -- | From "Utils.Drasil.English".
  capitalize, stringList,
  
  -- * Lists
  -- | From "Utils.Drasil.Lists". General functions involving lists.
  replaceAll, subsetOf, nubSort, weave,

  -- ** Strings
  toPlainName
) where

import Utils.Drasil.Document
import Utils.Drasil.English
import Utils.Drasil.Lists
import Utils.Drasil.Strings
