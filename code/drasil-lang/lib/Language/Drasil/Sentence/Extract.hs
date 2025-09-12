-- | Extract various kinds of UIDs from a Sentence. Used in conjunction with the
-- chunk database in order to render terms, symbols, and references properly.
module Language.Drasil.Sentence.Extract(sdep, shortdep, lnames, lnames') where

import Drasil.Database.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..))
import Language.Drasil.ModelExpr.Extract (meNames)

import Data.Containers.ListUtils (nubOrd)

-- | Generic traverse of all positions that could lead to /symbolic/ 'UID's from 'Sentence's.
getUIDs :: Sentence -> [UID]
getUIDs (Ch ShortStyle _ _) = []
getUIDs (Ch TermStyle _ _)  = []
getUIDs (Ch PluralTerm _ _) = []
getUIDs (SyCh a)            = [a]
getUIDs Sy {}               = []
getUIDs S {}                = []
getUIDs P {}                = []
getUIDs Ref {}              = []
getUIDs Percent             = []
getUIDs (a :+: b)           = getUIDs a ++ getUIDs b
getUIDs (Quote a)           = getUIDs a
getUIDs (E a)               = meNames a
getUIDs EmptyS              = []

-- | Generic traverse of all positions that could lead to /symbolic/ and
-- /abbreviated/ 'UID's from 'Sentence's but doesn't go into expressions.
getUIDshort :: Sentence -> [UID]
getUIDshort (Ch ShortStyle _ x) = [x]
getUIDshort (Ch TermStyle _ _)  = []
getUIDshort (Ch PluralTerm _ _) = []
getUIDshort SyCh {}             = []
getUIDshort Sy {}               = []
getUIDshort S {}                = []
getUIDshort Percent             = []
getUIDshort P {}                = []
getUIDshort (Ref _ s _)         = getUIDshort s
getUIDshort (a :+: b)           = getUIDs a ++ getUIDs b
getUIDshort (Quote a)           = getUIDshort a
getUIDshort E {}                = []
getUIDshort EmptyS              = []

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
-- | This is to collect /symbolic/ 'UID's that are printed out as a 'Symbol'.
sdep :: Sentence -> [UID]
sdep = nubOrd . getUIDs

-- This is to collect symbolic 'UID's that are printed out as an /abbreviation/.
shortdep :: Sentence -> [UID]
shortdep = nubOrd . getUIDshort

-- | Generic traverse of all positions that could lead to /reference/ 'UID's from 'Sentence's.
lnames :: Sentence -> [UID]
lnames Ch {}       = []
lnames SyCh {}     = []
lnames Sy {}       = []
lnames S {}        = []
lnames Percent     = []
lnames P {}        = []
lnames (Ref a _ _) = [a]
lnames (a :+: b)   = lnames a ++ lnames b
lnames Quote {}    = []
lnames E {}        = []
lnames EmptyS      = []

-- | Get /reference/ 'UID's from 'Sentence's.
lnames'  :: [Sentence] -> [UID]
lnames' = concatMap lnames
