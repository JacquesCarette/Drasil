-- | Extract various kinds of UIDs from a Sentence. Used in conjunction with the
-- chunk database in order to render terms, symbols, and references properly.
module Language.Drasil.Sentence.Extract(sdep, shortdep, lnames, lnames') where

import Data.Set (fromList, Set, empty, singleton, union)

import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..))
import Language.Drasil.ModelExpr.Extract (meNames)


-- | Generic traverse of all positions that could lead to /symbolic/ 'UID's from 'Sentence's.
getUIDs :: Sentence -> Set UID
getUIDs (Ch ShortStyle _ _) = empty
getUIDs (Ch TermStyle _ _)  = empty
getUIDs (Ch PluralTerm _ _) = empty
getUIDs (SyCh a)            = singleton a
getUIDs Sy {}               = empty
getUIDs S {}                = empty
getUIDs P {}                = empty
getUIDs Ref {}              = empty
getUIDs Percent             = empty
getUIDs ((:+:) a b)         = getUIDs a `union` getUIDs b
getUIDs (Quote a)           = getUIDs a
getUIDs (E a)               = fromList $ meNames a
getUIDs EmptyS              = empty

-- | Generic traverse of all positions that could lead to /symbolic/ and /abbreviated/ 'UID's from 'Sentence's
-- but doesn't go into expressions.
getUIDshort :: Sentence -> Set UID
getUIDshort (Ch ShortStyle _ a) = singleton a
getUIDshort (Ch TermStyle _ _)  = empty
getUIDshort (Ch PluralTerm _ _) = empty
getUIDshort SyCh {}             = empty
getUIDshort Sy {}               = empty
getUIDshort S {}                = empty
getUIDshort Percent             = empty
getUIDshort P {}                = empty
getUIDshort Ref {}              = empty
getUIDshort ((:+:) a b)         = getUIDshort a `union` getUIDshort b
getUIDshort (Quote a)           = getUIDshort a
getUIDshort E {}                = empty
getUIDshort EmptyS              = empty

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
-- | This is to collect /symbolic/ 'UID's that are printed out as a 'Symbol'.
sdep :: Sentence -> Set UID
sdep = getUIDs

-- This is to collect symbolic 'UID's that are printed out as an /abbreviation/.
shortdep :: Sentence -> Set UID
shortdep = getUIDshort

-- | Generic traverse of all positions that could lead to /reference/ 'UID's from 'Sentence's.
lnames :: Sentence -> [UID]
lnames Ch {}       = []
lnames SyCh {}     = []
lnames Sy {}       = []
lnames S {}        = []
lnames Percent     = []
lnames P {}        = []
lnames (Ref a _ _) = [a]
lnames ((:+:) a b) = lnames a ++ lnames b
lnames Quote {}    = []
lnames E {}        = []
lnames EmptyS      = []

-- | Get /reference/ 'UID's from 'Sentence's.
lnames'  :: [Sentence] -> [UID]
lnames' = concatMap lnames
