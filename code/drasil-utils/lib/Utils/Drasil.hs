-- | Gather Drasil's utility functions and re-export for easy use.
module Utils.Drasil (
  -- * Documents
  -- | From "Utils.Drasil.Document".
  blank, indent, indentList, filterEmpty, listToDoc,
  Separator, contSep,

  -- * Language
  -- | From "Utils.Drasil.English".
  capitalize, stringList,
  
  -- * Lists
  -- | From "Utils.Drasil.Lists". General functions involving lists.
  atLeast2, replaceAll, subsetOf, nubSort, weave,
  foldle, foldle1,
  toColumn,

  -- * Maps
  invert,

  -- ** Strings
  toPlainName
) where

import Utils.Drasil.Document
import Utils.Drasil.English
import Utils.Drasil.Lists
import Utils.Drasil.Maps
import Utils.Drasil.Strings
