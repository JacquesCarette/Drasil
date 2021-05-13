module Utils.Drasil.Phrase where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Control.Lens ((^.))

import Utils.Drasil.Sentence (sAnd, sOf, ofThe)

-- | Creates a NP by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural is defaulted to @(phrase t1) "of" (plural t2)@
and_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_ t1 t2 = nounPhrase''
  (phrase t1 `sAnd` phrase t2)
  (phrase t1 `sAnd` plural t2)
  (Replace (atStart t1 `sAnd` phrase t2))
  (Replace (titleize t1 `sAnd` titleize t2))

-- | Same as `and_` combinator, except phrase default of second term is plural instead of phrase
and_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_' t1 t2 = nounPhrase'' 
  (phrase t1 `sAnd` plural t2)
  (phrase t1 `sAnd` plural t2)
  (Replace (atStart t1 `sAnd` plural t2))
  (Replace (titleize t1 `sAnd` titleize' t2))

-- | Customizable `and_` combinator (takes two title case capitalization rules and two NamedIdeas)
andRT :: (NamedIdea c, NamedIdea d) => 
  (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
andRT f1 f2 t1 t2 = nounPhrase''
  (phrase t1 `sAnd` phrase t2)
  (phrase t1 `sAnd` plural t2)
  (Replace (atStart t1 `sAnd` phrase t2))
  (Replace (f1 t1 `sAnd` f2 t2))

-- | Case with "T1s with T2", as opposed to "T1 with T2", i.e.
-- phrase defaults to @(plural t1) "with" (phrase t2)@ while plural pluralizes both.
with :: (NamedIdea c, NamedIdea d) => c -> d -> NP
with t1 t2 = nounPhrase''
  (plural t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ plural t2)
  (Replace (atStart' t1 +:+ S "with" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "with" +:+ titleize t2))

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- their terms. Plural is defaulted to @(phrase t1) "of" (plural t2)@
of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = nounPhrase'' 
  (phrase t1 `sOf` phrase t2)
  (phrase t1 `sOf` plural t2)
  (Replace (atStart t1 `sOf` phrase t2))
  (Replace (titleize t1 `sOf` titleize t2))

-- | Same as 'of_' but second argument is a `NounPhrase`
ofN_ :: (NamedIdea c, NounPhrase d) => c -> d -> NP
ofN_ t1 t2 = nounPhrase'' 
  (phrase t1 `sOf` phraseNP t2)
  (phrase t1 `sOf` pluralNP t2)
  (Replace (atStart t1 `sOf` phraseNP t2))
  (Replace (titleize t1 `sOf` titleizeNP t2))

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- them. 'phrase' is defaulted to @(phrase t1) "of" (plural t2)@. Plural is the same.
of_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_' t1 t2 = nounPhrase'' 
  (phrase t1 `sOf` plural t2)
  (phrase t1 `sOf` plural t2)
  (Replace (atStart t1 `sOf` plural t2))
  (Replace (titleize t1 `sOf` titleize' t2))

-- | Same as 'of_', except plural is defaulted to @(plural t1) "of" (phrase t2)@
of_'' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_'' t1 t2 = nounPhrase'' 
  (phrase t1 `sOf` phrase t2)
  (plural t1 `sOf` phrase t2)
  (Replace (atStart t1 `sOf` phrase t2))
  (Replace (titleize t1 `sOf` titleize t2))

-- | Same as 'of_', except phrase and plural default to @(plural t1) "of" (phrase t2)@
of__ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of__ t1 t2 = nounPhrase'' 
  (plural t1 `sOf` phrase t2)
  (plural t1 `sOf` phrase t2)
  (Replace (atStart' t1 `sOf` phrase t2))
  (Replace (titleize' t1 `sOf` titleize t2))

-- | Same as 'of__', except combining Sentence piece is "of a"
ofA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofA t1 t2 = nounPhrase'' 
  (plural t1 +:+ S "of a" +:+ phrase t2)
  (plural t1 +:+ S "of a" +:+ phrase t2)
  (Replace (atStart' t1 +:+ S "of a" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "of a" +:+ titleize t2))

-- | Same as 'of_', except combining Sentence piece is "of the"
ofThe' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofThe' t1 t2 = nounPhrase'' 
  (phrase t1 `ofThe` phrase t2)
  (plural t1 `ofThe` phrase t2)
  (Replace (atStart' t1 `ofThe` phrase t2))
  (Replace (titleize' t1 `ofThe` titleize t2))

--FIXME: As mentioned in issue #487, the following should be re-examined later,
--       as they may embody a deeper idea in some cases.

-- we might want to eventually restrict the use of these via
-- some kind of type system, which asserts that:
-- 1. t1 `for` t2 means that t1 is a view of part of the reason behind t2
-- 2. t1 `of_` t2 means that t1 is a view of part of the structure of t2

-- | Inserts the word "for" between the titleized versions of
-- two terms
for :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for t1 t2 = titleize t1 +:+ S "for" +:+ titleize t2

-- | Similar to 'for', but uses titleized version of term 1 with the abbreviation
-- (if it exists, phrase otherwise) for term 2
for' :: (NamedIdea c, Idea d) => c -> d -> Sentence
for' t1 t2 = titleize t1 +:+ S "for" +:+ short t2

-- | Similar to 'for', but allows one to specify the function to use on each term
-- before inserting for. For example one could use @for'' phrase plural t1 t2@
for'' :: (c -> Sentence) -> (d -> Sentence) -> c -> d -> Sentence
for'' f1 f2 t1 t2 = f1 t1 +:+ S "for" +:+ f2 t2

-- | Prepends "the" to a titleized `NamedIdea`
the' :: (NamedIdea t) => t -> NP
the' t = nounPhrase'' (S "the" +:+ titleize t) (S "the" +:+ titleize' t) CapWords CapWords

-- | Prepends "the" to a `NamedIdea`. Similar to 'the'', but not titleized
the :: (NamedIdea t) => t -> NP
the t = nounPhrase'' (S "the" +:+ phrase t) (S "the" +:+ plural t) CapWords CapWords

-- | A customizable version of 'the'
theCustom :: (t -> Sentence) -> t -> NP
theCustom f t = nounPhrase''(S "the" +:+ f t) (S "the" +:+ f t) CapFirst CapWords

-- | Combinator for combining two 'NamedChunk's into one.
-- /Does not preserve abbreviations/
compoundNC :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC t1 t2 = nc
  (t1 ^. uid ++ t2^.uid) (compoundPhrase (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC' but uses 'pluralNP' for plural cases
compoundNC' :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC' t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase'' D.pluralNP D.pluralNP (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC', except plural cases are customizable
compoundNC'' :: (NamedIdea a, NamedIdea b) => 
  (NP -> Sentence) -> (NP -> Sentence) -> a -> b -> NamedChunk
compoundNC'' f1 f2 t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase'' f1 f2 (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC', except first parameter gets pluralized while second one stays singular
compoundNCPlPh :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCPlPh = compoundNC'' D.pluralNP D.phraseNP

-- | Same as 'compoundNC''
compoundNCPlPl :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCPlPl = compoundNC'' D.pluralNP D.pluralNP

-- hack for Solution Characteristics Specification, calling upon plural will pluralize
-- Characteristics as it is the end of the first term (solutionCharacteristic)
-- FIXME: need documentation for compoundPhrase''' before completeing this documentation
compoundNC''' :: (NamedIdea a, NamedIdea b) => (NP -> Sentence) -> a -> b -> NamedChunk
compoundNC''' f1 t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase''' f1 (t1 ^. term) (t2 ^. term))

-- FIXME: Same as above function
-- | Similar to 'compoundNC'''' but sets first parameter function to plural
compoundNCP1 :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCP1 = compoundNC''' D.pluralNP
