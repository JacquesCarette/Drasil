module Utils.Drasil.Concepts where

import Language.Drasil
import Utils.Drasil.Phrase
import Utils.Drasil.Sentence

-----------------
-- TO DO -> NOUNPHRASE HAS WRONG TYPE FOR THIS
----------------

--Maybe add generalized versions of these?
insertStringNP :: String -> NP -> NP -> NP 
insertStringNP s t1 t2 = nounPhrase (phraseNP t1 +:+ S s +:+ phraseNP t2) (phraseNP t1 +:+ S s +:+ pluralNP t2)

insertSentNP :: Sentence -> NP -> NP
insertSentNP s t1 t2 = nounPhrase (phraseNP t1 +:+ s +:+ phraseNP t2) (phraseNP t1 +:+ s +:+ pluralNP t2)


theNP :: NP -> NP
theNP t1 = nounPhrase (S "the" +:+ phraseNP t1) (S "the" +:+ pluralNP t1)

theNP' :: (NP -> Sentence) -> NP -> NP
theNP' f1 t1 = nounPhrase (S "the" +:+ f1 t1) (S "the" +:+ f1 t1)



aNP :: NP -> NP
aNP t1 = nounPhrase (S "a" +:+ phraseNP t1) (S "a" +:+ pluralNP t1)

aNP' :: (NP -> Sentence) -> NP -> NP
aNP' f1 t1 = nounPhrase (S "a" +:+ f1 t1) (S "a" +:+ f1 t1)



ofTheNP :: NP -> NP -> NP
ofTheNP t1 t2 = nounPhrase (phraseNP t1 `ofThe` phraseNP t2) (phraseNP t1 `ofThe` pluralNP t2)

ofTheNP' :: NP -> NP -> NP
ofTheNP' t1 t2 = nounPhrase (phraseNP t1 `ofThe` phraseNP t2) (pluralNP t1 `ofThe` phraseNP t2)

ofTheNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofTheNP'' f1 f2 t1 t2 = nounPhrase (phraseNP t1 `ofThe` phraseNP t2) (f1 t1 `ofThe` f2 t2)



the_ofTheNP :: NP -> NP -> NP
the_ofTheNP t1 t2 = theNP t1 `ofTheNP` t2

the_ofTheNP' :: NP -> NP -> NP
the_ofTheNP' t1 t2 = theNP t1 `ofTheNP'` t2

the_ofTheNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
the_ofTheNP'' f1 f2 t1 t2 = ofTheNP'' f1 f2 (theNP t1) t2



forNP :: NP -> NP -> NP
forNP t1 t2 = nounPhrase (phraseNP t1 +:+ S "for" +:+ phraseNP t2) (phraseNP t1 +:+ S "for" +:+ pluralNP t2) --Change to sFor

forNP' :: NP -> NP -> NP
forNP' t1 t2 = nounPhrase (phraseNP t1 +:+ S "for" +:+ phraseNP t2) (pluralNP t1 +:+ S "for" +:+ phraseNP t2)

forNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
forNP'' f1 f2 t1 t2 = nounPhrase (phraseNP t1 +:+ S "for" +:+ phraseNP t2) (f1 t1 +:+ S "for" +:+ f2 t2)



ofNP :: NP -> NP -> NP
ofNP t1 t2 = nounPhrase (phraseNP t1 `sOf` phraseNP t2) (phraseNP t1 `sOf` pluralNP t2)

ofNP' :: NP -> NP -> NP
ofNP' t1 t2 = nounPhrase (phraseNP t1 `sOf` phraseNP t2) (pluralNP t1 `sOf` phraseNP t2)

ofNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofNP'' f1 f2 t1 t2 = nounPhrase (phraseNP t1 `sOf` phraseNP t2) (f1 t1 `sOf` f2 t2)

ofNP''' :: (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
ofNP''' f1 f2 p1 p2 t1 t2 = nounPhrase (f1 t1 `sOf` f1 t2) (p1 t1 `sOf` p2 t2)



withNP :: NP -> NP -> NP
withNP t1 t2 = nounPhrase (phraseNP t1 +:+ S "with" +:+ phraseNP t2) (pluralNP t1 +:+ S "with" +:+ pluralNP t2)



andNP :: NP -> NP -> NP
andNP t1 t2 = nounPhrase (phraseNP t1 `sAnd` phraseNP t2) (phraseNP t1 `sAnd` pluralNP t2)

andNP' :: NP -> NP -> NP
andNP' t1 t2 = nounPhrase (phraseNP t1 `sAnd` phraseNP t2) (pluralNP t1 `sAnd` phraseNP t2)

andNP'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
andNP'' f1 f2 t1 t2 = nounPhrase (phraseNP t1 `sAnd` phraseNP t2) (f1 t1 `sAnd` f2 t2)

andNP''' :: (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
andNP''' f1 f2 p1 p2 t1 t2 = nounPhrase (f1 t1 `sAnd` f1 t2) (p1 t1 `sAnd` p2 t2)