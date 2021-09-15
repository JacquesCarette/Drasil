-- | Gather Drasil's utility functions and re-export for easy use.
-- For now, does not include combinators (Sentence.hs, NounPhrase.hs, Concepts.hs)
module Utils.Drasil (
  -- * Content-Related Utilities
  -- | From "Utils.Drasil.Contents".
  enumBullet, enumBulletU, enumSimple, enumSimpleU, mkEnumSimpleD,
  lbldExpr, unlbldExpr,
  -- * Document-Related Utilities
  -- | From "Utils.Drasil.Document".
  blank, indent, indentList,
  -- * Language-Related Utilities
  -- | From "Utils.Drasil.English".
  capitalize, stringList,
  -- * Fold-type Utilities.
  -- | From "Utils.Drasil.Fold". Defines many general fold functions
  -- for use with Drasil-related types.

  -- ** Folding Options as Types
  EnumType(..), WrapType(..), SepType(..), FoldType(..),
  -- ** Folding Functions
  -- *** Expression-related
  foldConstraints,
  -- *** Sentence-related
  foldlEnumList, foldlList, foldlSP, foldlSP_, foldlSPCol, foldlSent,
  foldlSent_,foldlSentCol, foldlsC, foldNums, numList,
  -- * List-type Utilities.
  -- | From "Utils.Drasil.Lists". General functions involving lists.
  replaceAll, subsetOf, nubSort,
  -- * Misc. Uitlities
  -- | From "Utils.Drasil.Misc". General sorting functions, useful combinators,
  -- and various functions to work with Drasil [Chunk](https://github.com/JacquesCarette/Drasil/wiki/Chunks) types.
  
  -- ** Reference-related Functions
  -- | Attach a 'Reference' and a 'Sentence' in different ways.
  chgsStart, definedIn, definedIn', definedIn'', definedIn''',
  eqnWSource, fromReplace, fromSource, fromSources, fmtU, follows,
  makeListRef,
  -- ** Sentence-related Functions
  -- | See Reference-related Functions as well.
  addPercent, displayStrConstrntsAsSet, displayDblConstrntsAsSet,
  eqN, checkValidStr, getTandS, maybeChanged, maybeExpanded,
  maybeWOVerb, showingCxnBw, substitute, typUncr, underConsidertn,
  unwrap, fterms,
  -- ** List-related Functions
  bulletFlat, bulletNested, itemRefToSent, makeTMatrix, mkEnumAbbrevList,
  mkTableFromColumns, noRefs, refineChain, sortBySymbol, sortBySymbolTuple,
  tAndDOnly, tAndDWAcc, tAndDWSym,
  weave, zipSentList
  -- Concepts
    -- may want to add concept-level combinators back into Drasil.Utils,
    -- but for now leave as a separate and exposed module.
) where

import Utils.Drasil.Contents
import Utils.Drasil.Document
import Utils.Drasil.English
import Utils.Drasil.Fold
import Utils.Drasil.Lists
import Utils.Drasil.Misc
--import Utils.Drasil.Concepts
