module Utils.Drasil.NounPhrase (insertString, prependString, insertSent, prependSent,
the, theGen, a_, a_Gen, ofThe, ofThePS, ofTheGen, inThe, inThePS, inTheGen, the_ofThe, the_ofThePS, the_ofTheGen,
for, forPS, forGen, of_, of_PS, of_Gen, of_GenGen, with, and_, and_PS, and_Gen, and_GenGen) where

import Language.Drasil
--import Utils.Drasil.Phrase
import qualified Utils.Drasil.Sentence as S


--Maybe move these to a separate Drasil.NounPhrase section
-- | Helper function that places a 'String' in between two 'NP's. Plural case is
-- @(phraseNP t1) +:+ S s +:+ (pluralNP t2)@.
insertString :: String -> NP -> NP -> NP 
insertString s t1 t2 = nounPhrase'' (phraseNP t1 +:+ S s +:+ phraseNP t2) (phraseNP t1 +:+ S s +:+ pluralNP t2) CapFirst CapWords
-- | Helper function that prepends a 'String' to a 'NP'.
prependString :: String -> NP -> NP
prependString s t1 = nounPhrase'' (S s +:+ phraseNP t1) (S s +:+ pluralNP t1) CapFirst CapWords
-- | Helper function that places a 'Sentence' in between two 'NP's. Plural case is
-- @(phraseNP t1) +:+ s +:+ (pluralNP t2)@.
insertSent :: Sentence -> NP -> NP -> NP
insertSent s t1 t2 = nounPhrase'' (phraseNP t1 +:+ s +:+ phraseNP t2) (phraseNP t1 +:+ s +:+ pluralNP t2) CapFirst CapWords
-- | Helper function that prepends a 'Sentence' to a 'NP'.
prependSent :: Sentence -> NP -> NP
prependSent s t1 = nounPhrase'' (s +:+ phraseNP t1) (s +:+ pluralNP t1) CapFirst CapWords


-- | Prepends "the" to a 'NP'.
the :: NP -> NP
the = prependString "the" 
-- | Similar to 'the', but accepts a function that determines the plural case.
theGen :: (NP -> Sentence) -> NP -> NP
theGen f1 t1 = nounPhrase'' (S "the" +:+ phraseNP t1) (S "the" +:+ f1 t1) CapFirst CapWords

-- | Prepends "a" to a 'NP'.
a_ :: NP -> NP
a_ = prependString "a" 
-- | Similar to 'a', but accepts a function that determines the plural case.
a_Gen :: (NP -> Sentence) -> NP -> NP
a_Gen f1 t1 = nounPhrase'' (S "a" +:+ phraseNP t1) (S "a" +:+ f1 t1) CapFirst CapWords

-- | Inserts "of the" between two 'NP's. Plural case is @(phraseNP t1) +:+ "of the" +:+ (pluralNP t2)@.
ofThe :: NP -> NP -> NP
ofThe = insertString "of the"
-- | Similar to 'ofThe', but the plural case is now @(pluralNP t1) `S.ofThe` (phraseNP t2)@.
ofThePS :: NP -> NP -> NP
ofThePS t1 t2 = nounPhrase'' (phraseNP t1 `S.ofThe` phraseNP t2) (pluralNP t1 `S.ofThe` phraseNP t2) CapFirst CapWords
-- | Similar to 'ofThe', but accepts two functions for the plural case.
ofTheGen :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofTheGen f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `S.ofThe` phraseNP t2) (f1 t1 `S.ofThe` f2 t2) CapFirst CapWords

-- | Inserts "in the" between two 'NP's. Plural case is @(phraseNP t1) +:+ "in the" +:+ (pluralNP t2)@.
inThe :: NP -> NP -> NP
inThe = insertString "in the"
-- | Similar to 'ofThe', but the plural case is now @(pluralNP t1) `S.inThe` (phraseNP t2)@.
inThePS :: NP -> NP -> NP
inThePS t1 t2 = nounPhrase'' (phraseNP t1 `S.inThe` phraseNP t2) (pluralNP t1 `S.inThe` phraseNP t2) CapFirst CapWords
-- | Similar to 'ofThe', but accepts two functions for the plural case.
inTheGen :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
inTheGen f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `S.inThe` phraseNP t2) (f1 t1 `S.inThe` f2 t2) CapFirst CapWords

-- | Prepends "the" and inserts "of the". Plural case is @"the" +:+ (phraseNP t1) +:+ "of the" +:+ (pluralNP t2)@.
the_ofThe :: NP -> NP -> NP
the_ofThe t1 t2 = the t1 `ofThe` t2
-- | Similar to 'the_ofThe', but the plural case is now @ S "the" +:+ (pluralNP t1) `S.ofThe` (phraseNP t2)@.
the_ofThePS :: NP -> NP -> NP
the_ofThePS t1 t2 = the t1 `ofThePS` t2
-- | Similar to 'the_ofThe'', but takes two functions for the plural case.
the_ofTheGen :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
the_ofTheGen f1 f2 t1 = ofTheGen f1 f2 (the t1)

-- | Inserts "for" between two 'NP's. Plural case is @(phraseNP t1) +:+ "for" +:+ (pluralNP t2)@.
for :: NP -> NP -> NP
for = insertString "for"
-- | Same as 'for', but plural case is now @(pluralNP t1) `S.for` (phraseNP t2)@.
forPS :: NP -> NP -> NP
forPS t1 t2 = nounPhrase'' (phraseNP t1 `S.for` phraseNP t2) (pluralNP t1 `S.for` phraseNP t2) CapFirst CapWords
-- | Same as 'for'', but takes two functions for the plural case.
forGen :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
forGen f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `S.for` phraseNP t2) (f1 t1 `S.for` f2 t2) CapFirst CapWords

-- | Inserts "of" between two 'NP's. Plural case is @(phraseNP t1) +:+ "of" +:+ (pluralNP t2)@.
of_ :: NP -> NP -> NP
of_ = insertString "of"
-- | Same as 'of_', but plural case is now @(pluralNP t1) `S.of_` (phraseNP t2)@.
of_PS :: NP -> NP -> NP
of_PS t1 t2 = nounPhrase'' (phraseNP t1 `S.of_` phraseNP t2) (pluralNP t1 `S.of_` phraseNP t2) CapFirst CapWords
-- | Same as 'of_', but takes two functions for the plural case.
of_Gen :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
of_Gen f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `S.of_` phraseNP t2) (f1 t1 `S.of_` f2 t2) CapFirst CapWords
-- | Same as 'of_', but takes two functions for the singular case and two for the plural case.
of_GenGen :: (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
of_GenGen f1 f2 p1 p2 t1 t2 = nounPhrase'' (f1 t1 `S.of_` f2 t2) (p1 t1 `S.of_` p2 t2) CapFirst CapWords

-- | Inserts "with" between two 'NP's. Plural case is @(phraseNP t1) +:+ "with" +:+ (pluralNP t2)@.
with :: NP -> NP -> NP
with = insertString "with"

-- | Inserts "and" between two 'NP's. Plural case is @(phraseNP t1) +:+ "and" +:+ (pluralNP t2)@.
and_ :: NP -> NP -> NP
and_ = insertString "and"
-- | Same as 'and_', but plural case is now @(pluralNP t1) `S.and_` (phraseNP t2)@.
and_PS :: NP -> NP -> NP
and_PS t1 t2 = nounPhrase'' (phraseNP t1 `S.and_` phraseNP t2) (pluralNP t1 `S.and_` phraseNP t2) CapFirst CapWords
-- | Same as 'and_', but takes two functions for the plural case.
and_Gen :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
and_Gen f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `S.and_` phraseNP t2) (f1 t1 `S.and_` f2 t2) CapFirst CapWords
-- | Same as 'and_', but takes two functions for the singular case and two for the plural case.
and_GenGen :: (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
and_GenGen f1 f2 p1 p2 t1 t2 = nounPhrase'' (f1 t1 `S.and_` f2 t2) (p1 t1 `S.and_` p2 t2) CapFirst CapWords