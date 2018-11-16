module Data.Drasil.Phrase where
import Language.Drasil
import qualified Language.Drasil.Development as D
import Control.Lens ((^.))

-- | Creates an NP by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural is defaulted to @(phrase t1) "of" (plural t2)@
and_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_ t1 t2 = nounPhrase''
  ((phrase t1) +:+ S "and" +:+ (phrase t2))
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  (Replace ((at_start t1) +:+ S "and" +:+ (phrase t2)))
  (Replace ((titleize t1) +:+ S "and" +:+ (titleize t2)))

-- | Same as `and_` combinator, except phrase default of second term is plural instead of phrase
and_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_' t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  (Replace ((at_start t1) +:+ S "and" +:+ (plural t2)))
  (Replace ((titleize t1) +:+ S "and" +:+ (titleize' t2)))

-- | Customizable `and` combinator
andRT :: (NamedIdea c, NamedIdea d) => 
  (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
andRT f1 f2 t1 t2 = nounPhrase''
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  ((phrase t1) +:+ S "and" +:+ (phrase t2))
  (Replace ((at_start t1) +:+ S "and" +:+ (phrase t2)))
  (Replace ((f1 t1) +:+ S "and" +:+ (f2 t2)))

-- Case with "T1s with T2", as opposed to "T1 with T2", i.e.
-- phrase defaults to @(plural t1) "with" (phrase t2)@, plural pluralizes both.
with :: (NamedIdea c, NamedIdea d) => c -> d -> NP
with t1 t2 = nounPhrase''
  (plural t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ plural t2)
  (Replace (at_start' t1 +:+ S "with" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "with" +:+ titleize t2))

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- their terms. Plural is defaulted to @(phrase t1) "of" (plural t2)@
of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "of" +:+ (phrase t2))
  ((phrase t1) +:+ S "of" +:+ (plural t2))
  (Replace ((at_start t1) +:+ S "of" +:+ (phrase t2)))
  (Replace ((titleize t1) +:+ S "of" +:+ (titleize t2)))

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- them. 'phrase' is defaulted to @(phrase t1) "of" (plural t2)@. Plural is the same.
of_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_' t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "of" +:+ (plural t2))
  ((phrase t1) +:+ S "of" +:+ (plural t2))
  (Replace ((at_start t1) +:+ S "of" +:+ (plural t2)))
  (Replace ((titleize t1) +:+ S "of" +:+ (titleize' t2)))

-- | Same as of_, except plural default of second term is phrase
of_'' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_'' t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "of" +:+ (phrase t2))
  ((plural t1) +:+ S "of" +:+ (phrase t2))
  (Replace ((at_start t1) +:+ S "of" +:+ (phrase t2)))
  (Replace ((titleize t1) +:+ S "of" +:+ (titleize t2)))

-- | Same as of_, except phrase default of first term is plural instead of phrase
of__ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of__ t1 t2 = nounPhrase'' 
  ((plural t1) +:+ S "of" +:+ (phrase t2))
  ((plural t1) +:+ S "of" +:+ (phrase t2))
  (Replace ((at_start' t1) +:+ S "of" +:+ (phrase t2)))
  (Replace ((titleize' t1) +:+ S "of" +:+ (titleize t2)))

-- | Same as of__, except combining Sentence piece is "of a"
ofA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofA t1 t2 = nounPhrase'' 
  ((plural t1) +:+ S "of a" +:+ (phrase t2))
  ((plural t1) +:+ S "of a" +:+ (phrase t2))
  (Replace ((at_start' t1) +:+ S "of a" +:+ (phrase t2)))
  (Replace ((titleize' t1) +:+ S "of a" +:+ (titleize t2)))

--FIXME: As mentioned in issue #487, the following should be re-examined later,
--       as they may embody a deeper idea in some cases.

-- we might want to eventually restrict the use of these via
-- some kind of type system, which asserts that:
-- 1. t1 `for` t2 means that t1 is a view of part of the reason behind t2
-- 2. t1 `of_` t2 means that t1 is a view of part of the structure of t2

-- | Inserts the word "for" between the titleized versions of
-- two terms
for :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for t1 t2 = (titleize t1) +:+ S "for" +:+ (titleize t2)

-- | Similar to 'for', but uses titleized version of term 1 with the abbreviation
-- (if it exists, phrase otherwise) for term 2
for' :: (NamedIdea c, Idea d) => c -> d -> Sentence
for' t1 t2 = (titleize t1) +:+ S "for" +:+ (short t2)

-- | Similar to 'for', but allows one to specify the function to use on each term
-- before inserting for. For example one could use @for'' phrase plural t1 t2@
for'' :: (NamedIdea c, NamedIdea d) => (c -> Sentence) -> (d -> Sentence) -> c -> d -> Sentence
for'' f1 f2 t1 t2 = (f1 t1) +:+ S "for" +:+ (f2 t2)

the :: (NamedIdea t) => t -> NP
the t = nounPhrase'' (S "the" +:+ phrase t) (S "the" +:+ plural t) CapWords CapWords

{--the :: (NamedIdea t) => t -> NamedChunk
the t = nc ("the" ++ t ^. uid)
  (nounPhrase'' (S "the" +:+ phrase t) (S "the" +:+ plural t) CapFirst CapWords)--}

theCustom :: (NamedIdea t) => (t -> Sentence) -> t -> NamedChunk
theCustom f t = nc ("the" ++ t ^. uid) (nounPhrase''(S "the" +:+ f t)
  (S "the" +:+ f t) CapFirst CapWords)

-- | Combinator for combining two 'NamedChunk's into one.
-- /Does not preserve abbreviations/
compoundNC :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC t1 t2 = nc
  (t1^.uid ++ t2^.uid) (compoundPhrase (t1 ^. term) (t2 ^. term))
  
compoundNC' :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC' t1 t2 = nc
  (t1^.uid ++ t2^.uid) (compoundPhrase'' D.pluralNP D.pluralNP (t1 ^. term) (t2 ^. term))
  
compoundNC'' :: (NamedIdea a, NamedIdea b) => 
  (NP -> Sentence) -> (NP -> Sentence) -> a -> b -> NamedChunk
compoundNC'' f1 f2 t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase'' f1 f2 (t1 ^. term) (t2 ^. term))

compoundNCPlPh :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCPlPh = compoundNC'' D.pluralNP D.phraseNP

compoundNCPlPl :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCPlPl = compoundNC'' D.pluralNP D.pluralNP

-- hack for Solution Characteristics Specification, calling upon plural will pluralize
-- Characteristics as it is the end of the first term (solutionCharacteristic)
compoundNC''' :: (NamedIdea a, NamedIdea b) => (NP -> Sentence) -> a -> b -> NamedChunk
compoundNC''' f1 t1 t2 = nc
  (t1^.uid ++ t2^.uid) (compoundPhrase''' f1 (t1 ^. term) (t2 ^. term))

compoundNCP1 :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCP1 = compoundNC''' D.pluralNP

