module Utils.Drasil.Concepts (insertStringNP, prependStringNP, insertSentNP, prependSentNP,
theNP, theNP', aNP, aNP', ofTheNP, ofTheNP', ofTheNP'', inTheNP, inTheNP', inTheNP'', the_ofTheNP, the_ofTheNP', the_ofTheNP'',
forNP, forNP', forNP'', ofNP, ofNP', ofNP'', ofNP''', withNP, andNP, andNP', andNP'', andNP''',
combineNINP, combineNPNI)where

import Language.Drasil
--import Utils.Drasil.Phrase
import Utils.Drasil.Sentence


--Maybe add more generalized versions of these?
-- | Helper function that places a string in between two 'NP's. Plural is defaulted to
-- @phraseNP t1 +:+ S s +:+ pluralNP t2@
insertStringNP :: String -> NP -> NP -> NP 
insertStringNP s t1 t2 = nounPhrase'' (phraseNP t1 +:+ S s +:+ phraseNP t2) (phraseNP t1 +:+ S s +:+ pluralNP t2) CapFirst CapWords
-- | Helper function that prepends a string to a 'NP'
prependStringNP :: String -> NP -> NP
prependStringNP s t1 = nounPhrase'' (S s +:+ phraseNP t1) (S s +:+ pluralNP t1) CapFirst CapWords
-- | Helper function that places a Sentence in between two 'NP's. Plural is defaulted to
-- @phraseNP t1 +:+ s +:+ pluralNP t2@
insertSentNP :: Sentence -> NP -> NP -> NP
insertSentNP s t1 t2 = nounPhrase'' (phraseNP t1 +:+ s +:+ phraseNP t2) (phraseNP t1 +:+ s +:+ pluralNP t2) CapFirst CapWords
-- | Helper function that prepends a 'Sentence' to a 'NP'
prependSentNP :: Sentence -> NP -> NP
prependSentNP s t1 = nounPhrase'' (s +:+ phraseNP t1) (s +:+ pluralNP t1) CapFirst CapWords

-- | Helper function that combines a 'NamedIdea' and a 'NP' without any words in between.Plural is defaulted
-- to @phrase t1 +:+ pluralNP t2@
combineNINP :: (NamedIdea c) => c -> NP -> NP
combineNINP t1 t2 = nounPhrase'' (phrase t1 +:+ phraseNP t2) (phrase t1 +:+ pluralNP t2) CapFirst CapWords
-- | Similar to 'combineNINP' but takes in a 'NP' first and a 'NamedIdea' second
combineNPNI :: (NamedIdea c) => NP -> c -> NP
combineNPNI t1 t2 = nounPhrase'' (phraseNP t1 +:+ phrase t2) (phraseNP t1 +:+ plural t2) CapFirst CapWords

-- | Prepends "the" to a 'NP'
theNP :: NP -> NP
theNP = prependStringNP "the" 
-- | Similar to 'theNP', but accepts a function that determines the plural case
theNP' :: (NP -> Sentence) -> NP -> NP
theNP' f1 t1 = nounPhrase'' (S "the" +:+ phraseNP t1) (S "the" +:+ f1 t1) CapFirst CapWords

-- | Prepends "a" to a 'NP'
aNP :: NP -> NP
aNP = prependStringNP "a" 
-- | Similar to 'aNP', but accepts a function that determines the plural case
aNP' :: (NP -> Sentence) -> NP -> NP
aNP' f1 t1 = nounPhrase'' (S "a" +:+ phraseNP t1) (S "a" +:+ f1 t1) CapFirst CapWords

-- | Inserts "of the" between two 'NP's
ofTheNP :: NP -> NP -> NP
ofTheNP = insertStringNP "of the"
-- | Similar to 'ofTheNP', but the plural case is now @pluralNP t1 `ofThe` phraseNP t2@
ofTheNP' :: NP -> NP -> NP
ofTheNP' t1 t2 = nounPhrase'' (phraseNP t1 `ofThe` phraseNP t2) (pluralNP t1 `ofThe` phraseNP t2) CapFirst CapWords
-- | Similar to 'ofTheNP', but accepts two functions for the plural case
ofTheNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofTheNP'' f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `ofThe` phraseNP t2) (f1 t1 `ofThe` f2 t2) CapFirst CapWords

-- | Inserts "in the" between two 'NP's
inTheNP :: NP -> NP -> NP
inTheNP = insertStringNP "in the"
-- | Similar to 'ofTheNP', but the plural case is now @pluralNP t1 `inThe` phraseNP t2@
inTheNP' :: NP -> NP -> NP
inTheNP' t1 t2 = nounPhrase'' (phraseNP t1 `inThe` phraseNP t2) (pluralNP t1 `inThe` phraseNP t2) CapFirst CapWords
-- | Similar to 'ofTheNP', but accepts two functions for the plural case
inTheNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
inTheNP'' f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `inThe` phraseNP t2) (f1 t1 `inThe` f2 t2) CapFirst CapWords

-- | Prepends "the" and inserts "of the"
the_ofTheNP :: NP -> NP -> NP
the_ofTheNP t1 t2 = theNP t1 `ofTheNP` t2
-- | Similar to 'the_ofTheNP', but the plural case is now @ S "the" +:+ pluralNP t1 `ofThe` phraseNP t2@
the_ofTheNP' :: NP -> NP -> NP
the_ofTheNP' t1 t2 = theNP t1 `ofTheNP'` t2
-- | Similar to 'the_ofTheNP'', but takes two functions for the plural case
the_ofTheNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
the_ofTheNP'' f1 f2 t1 = ofTheNP'' f1 f2 (theNP t1)

-- | Inserts "for" between two 'NP's
forNP :: NP -> NP -> NP
forNP = insertStringNP "for"
--FIXME: Change "for" to sFor
-- | Same as 'forNP', but plural case is now @pluralNP t1 `sFor` phraseNP t2@
forNP' :: NP -> NP -> NP
forNP' t1 t2 = nounPhrase'' (phraseNP t1 +:+ S "for" +:+ phraseNP t2) (pluralNP t1 +:+ S "for" +:+ phraseNP t2) CapFirst CapWords
-- | Same as 'forNP'', but takes two functions for the plural case
forNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
forNP'' f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 +:+ S "for" +:+ phraseNP t2) (f1 t1 +:+ S "for" +:+ f2 t2) CapFirst CapWords

-- | Inserts "of" between two 'NP's
ofNP :: NP -> NP -> NP
ofNP = insertStringNP "of"
-- | Same as 'ofNP', but plural case is now @pluralNP t1 `sOf` phraseNP t2@
ofNP' :: NP -> NP -> NP
ofNP' t1 t2 = nounPhrase'' (phraseNP t1 `sOf` phraseNP t2) (pluralNP t1 `sOf` phraseNP t2) CapFirst CapWords
-- | Same as 'ofNP', but takes two functions for the plural case
ofNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofNP'' f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `sOf` phraseNP t2) (f1 t1 `sOf` f2 t2) CapFirst CapWords
-- | Same as 'ofNP', but takes two functions for the singular case and two for the plural case
ofNP''' :: (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofNP''' f1 f2 p1 p2 t1 t2 = nounPhrase'' (f1 t1 `sOf` f2 t2) (p1 t1 `sOf` p2 t2) CapFirst CapWords

-- | Inserts "with" between two 'NP's
withNP :: NP -> NP -> NP
withNP = insertStringNP "with"

-- | Inserts "and" between two 'NP's
andNP :: NP -> NP -> NP
andNP = insertStringNP "and"
-- | Same as 'andNP', but plural case is now @pluralNP t1 `sAnd` phraseNP t2@
andNP' :: NP -> NP -> NP
andNP' t1 t2 = nounPhrase'' (phraseNP t1 `sAnd` phraseNP t2) (pluralNP t1 `sAnd` phraseNP t2) CapFirst CapWords
-- | Same as 'andNP', but takes two functions for the plural case
andNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
andNP'' f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 `sAnd` phraseNP t2) (f1 t1 `sAnd` f2 t2) CapFirst CapWords
-- | Same as 'andNP', but takes two functions for the singular case and two for the plural case
andNP''' :: (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
andNP''' f1 f2 p1 p2 t1 t2 = nounPhrase'' (f1 t1 `sAnd` f2 t2) (p1 t1 `sAnd` p2 t2) CapFirst CapWords