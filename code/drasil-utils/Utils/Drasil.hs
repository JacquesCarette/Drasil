module Utils.Drasil (
  -- Contents
  enumBullet, enumBulletU, enumSimple, enumSimpleU, mkEnumSimpleD,
  lbldExpr, unlbldExpr,
  -- Document
  blank, indent, indentList,
  -- English
  capitalize, stringList,
  -- Fold
  EnumType(..), WrapType(..), SepType(..), FoldType(..), foldConstraints,
  foldlEnumList, foldlList, foldlSP, foldlSP_, foldlSPCol, foldlSent,
  foldlSent_,foldlSentCol, foldlsC, foldNums, numList,
  -- Lists
  replaceAll,
  -- Misc
  addPercent, bulletFlat, bulletNested, checkValidStr, chgsStart, definedIn,
  definedIn', definedIn'', definedIn''', displayStrConstrntsAsSet, displayDblConstrntsAsSet, eqN,
  eqnWSource, fromReplace, fromSource, fromSources, fmtU, follows, getTandS,
  itemRefToSent, makeListRef, makeTMatrix, maybeChanged, maybeExpanded,
  maybeWOVerb, mkEnumAbbrevList, mkTableFromColumns, noRefs, refineChain,
  showingCxnBw, sortBySymbol, sortBySymbolTuple, substitute, tAndDOnly,
  tAndDWAcc, tAndDWSym, typUncr, underConsidertn, unwrap, weave, zipSentList, fterms,
  -- Concepts
  --and_, and_', andRT, compoundNC, compoundNC', compoundNC'', compoundNC''',
  --compoundNCP1, compoundNCPlPh, for, for', of_, of_',
  --of_'', of__, ofA, ofA', ofN_, the, the', the'', aNINP, aNINP', inThe', with, ofThe', the_ofThe'', onThe', 
  --combineNINP, combineNPNI
  -- Sentence
  --andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, the_ofThe, the_ofThe',
  --sOf, sOfA, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe, sFor, sFor', sFor'', forTT, forTT'
  -- NounPhrase
  --insertStringNP, prependStringNP, insertSentNP, prependSentNP,
  --theNP, theNP', aNP, aNP', ofTheNP, ofTheNP', ofTheNP'', inTheNP, inTheNP', inTheNP'', the_ofTheNP, the_ofTheNP', the_ofTheNP'',
  --forNP, forNP', forNP'', ofNP, ofNP', ofNP'', ofNP''', withNP, andNP, andNP', andNP'', andNP''',
) where

import Utils.Drasil.Contents
import Utils.Drasil.Document
import Utils.Drasil.English
import Utils.Drasil.Fold
import Utils.Drasil.Lists
import Utils.Drasil.Misc
--import Utils.Drasil.Concepts
--import Utils.Drasil.NounPhrase
--import Utils.Drasil.Sentence
