-- | Defines various chunk combinators. The full naming scheme can be found
-- in the [Wiki](https://github.com/JacquesCarette/Drasil/wiki/Combinator-Documentation).
-- For convenience, here is a summary:
--
--    * Combinators that conflict with haskell-native functions have an underscore appended.
--    * Default pluralNP case for combinators will be first term singular, second term plural.
--    * @P@ and @S@ denote the pluralNP case of the combinator when it does not follow the above default.
--    * @Gen@ denotes the general function case.
--    * Although this should eventually be phased out, @T@ denotes a combinator meant for use with titles.
--    * @NI@ and @NP@ denote whether something must be a part of the 'NamedIdea' or 'NounPhrase' class.
module Language.Drasil.Chunk.Concept.NamedCombinators (
  -- * Prepositions
  -- ** \"The\" Combinators
  the, theGen,
  -- ** \"A\" Combinators
  a_, a_Gen,
  -- * Conjunctions
  -- ** \"And\" Combinators
  and_, and_PS, and_PP, and_TGen,
  andIts, andThe,
  -- ** \"Of\" Combinators
  of_, of_NINP, of_PSNPNI, of_PS, ofA, ofAPS, ofThe, ofThePS,
  -- ** \"The\" Combinators
  the_ofThe, the_ofThePS, onThe, onThePS, onThePP,
  inThe, inThePS, inThePP, isThe, toThe,
  -- ** \"For\" Combinators
  for, forTGen,
  -- ** \"In\" Combinators
  in_, in_PS, inA,
  -- ** Other Combinators
  is, with,
  -- * Direct Term Combinators
  -- | Some are specific to 'IdeaDict's.
  compoundNC, compoundNCPP, compoundNCGen,
  compoundNCPS, compoundNCPSPP, compoundNCGenP,
  combineNINP, combineNINI
) where

import Control.Lens ((^.))

import Drasil.Database ((+++!))

import Language.Drasil.Chunk.NamedIdea (IdeaDict, ncUID)
import Language.Drasil.Classes (Idea, NamedIdea(..))
import Language.Drasil.NounPhrase (NP, CapitalizationRule(CapWords, Replace,
  CapFirst), NounPhrase(phraseNP, pluralNP), nounPhrase'', compoundPhrase,
  compoundPhrase'', compoundPhrase''')
import Language.Drasil.NounPhrase.Types (NPStruct(S,(:+:)))
import qualified Language.Drasil.NounPhrase as D (NounPhrase(pluralNP, phraseNP))
import qualified Language.Drasil.NounPhrase.Combinators as NP (
  insertString, insertStringOp, insertStringGen)

-- not exported
phrase, plural :: NamedIdea n => n -> NPStruct
phrase k = phraseNP $ k ^. term
plural k = pluralNP $ k ^. term

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural case is @(phraseNP t1) "and" (pluralNP t2)@.
and_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_ t1 t2 = NP.insertString "and" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural case is @(pluralNP t1) "and" (phraseNP t2)@.
and_PS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_PS t1 t2 = NP.insertStringOp "and" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural case is @(pluralNP t1) "and" (pluralNP t2)@.
and_PP :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_PP t1 t2 = nounPhrase''
  (phrase t1 :+: S "and" :+: phrase t2)
  (plural t1 :+: S "and" :+: plural t2)
  CapFirst
  CapWords

-- | Customizable `and_` combinator (takes two title case capitalization rules and two 'NamedIdeas').
and_TGen :: (NamedIdea c, NamedIdea d) =>
  (c -> NPStruct) -> (d -> NPStruct) -> c -> d -> NP
and_TGen f1 f2 t1 t2 = nounPhrase''
  (phrase t1 :+: S "and" :+: phrase t2)
  (plural t1 :+: S "and" :+: plural t2)
  CapFirst
  (Replace (f1 t1 :+: S "and" :+: f2 t2))

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "and its" between
-- their terms. Plural case is @(phraseNP t1) "and its" (pluralNP t2)@.
andIts :: (NamedIdea a, NamedIdea b) => a -> b -> NP
andIts t1 t2 = NP.insertString "and its" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "and the" between
-- their terms. Plural case is @(phraseNP t1) "and the" (pluralNP t2)@.
andThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
andThe t1 t2 = NP.insertString "and the" (t1 ^. term) (t2 ^. term)

-- | Case with "T1s with T2", as opposed to "T1 with T2", i.e.
-- singular case is @(pluralNP t1) "with" (phraseNP t2)@ while the pluralNP case pluralizes the first.
with :: (NamedIdea c, NamedIdea d) => c -> d -> NP
with t1 t2 = NP.insertString "with" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "of" between
-- their terms. Plural case is @(phraseNP t1) "of" (pluralNP t2)@.
of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = NP.insertString "of" (t1 ^. term) (t2 ^. term)

-- | Same as 'of_' but second argument is NP.
of_NINP :: (NamedIdea c) => c -> NP -> NP
of_NINP t1 = NP.insertString "of" (t1 ^. term)

-- | Same as 'of_' but first argument is a `NounPhrase`
-- and pluralNP case is @(pluralNP t1) "of" (phraseNP t2)@.
of_PSNPNI :: (NamedIdea d) => NP -> d -> NP
of_PSNPNI t1 t2 = NP.insertStringOp "of" t1 (t2 ^. term)

-- | Same as 'of_', except pluralNP case is @(pluralNP t1) "of" (phraseNP t2)@.
of_PS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_PS t1 t2 = NP.insertStringOp "of" (t1 ^. term) (t2 ^. term)

-- | Same as 'of_PS', except combining 'NPStruct piece is "of a".
ofA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofA t1 t2 = NP.insertStringOp "of a" (t1 ^. term) (t2 ^. term)

--- | Same as 'ofA', except phrase case is @(plural t1) "of a" (phrase t2)@.
ofAPS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofAPS t1 t2 = NP.insertStringOp "of a" (t1 ^. term) (t2 ^. term)

-- | Same as 'of_', except combining 'NPStruct piece is "of the". Plural case is @(phraseNP t1) `NP.ofThe` (pluralNP t2)@.
ofThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofThe t1 t2 = NP.insertString "of the" (t1 ^. term) (t2 ^. term)

-- | Same as 'ofThe', except pluralNP case is @(pluralNP t1) `NP.ofThe` (phraseNP t2)@.
ofThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofThePS t1 t2 = NP.insertStringOp "of the" (t1 ^. term) (t2 ^. term)

-- | Same as 'ofThe', except prepends "the".
the_ofThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
the_ofThe t1 t2 = NP.insertString "of the" (the t1) (t2 ^. term)

-- | Same as 'the_ofThe', except pluralNP case is @(pluralNP t1) `NP.the_ofThe` (phraseNP t2)@
the_ofThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
the_ofThePS t1 t2 = NP.insertStringOp "of the" (the t1) (t2 ^. term)

-- | Same as 'of_', except combining NPStruct piece is "on the".
onThe :: (NamedIdea a, NamedIdea b) => a -> b -> NP
onThe t1 t2 = NP.insertString "on the" (t1 ^. term) (t2 ^. term)

-- | Same as 'onThe', except pluralNP case is (pluralNP t1) NP.onThe (phraseNP t2)
onThePS :: (NamedIdea a, NamedIdea b) => a -> b -> NP
onThePS t1 t2 = NP.insertStringOp "on the" (t1 ^. term) (t2 ^. term)

-- | Same as 'onThe', except pluralNP case is (pluralNP t1) NP.onThe (pluralNP t2)
onThePP :: (NamedIdea c, NamedIdea d) => c -> d -> NP
onThePP t1 t2 = NP.insertStringGen "on the" pluralNP pluralNP (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in the" between
-- their terms. Plural case is @(phraseNP t1) "in the" (pluralNP t2)@.
inThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inThe t1 t2 = NP.insertString "in the" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in the" between
-- their terms. Plural case is @(pluralNP t1) "in the" (phraseNP t2)@.
inThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inThePS t1 t2 = NP.insertStringOp "in the" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in the" between
-- their terms. Plural case is @(pluralNP t1) "in the" (pluralNP t2)@.
inThePP :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inThePP t1 t2 = NP.insertStringGen "in the" pluralNP pluralNP (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "is the" between
-- their terms. Plural case is @(phraseNP t1) "is the" (pluralNP t2)@.
isThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
isThe t1 t2 = NP.insertString "is the" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "to the" between
-- their terms. Plural case is @(phraseNP t1) "to the" (pluralNP t2)@.
toThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
toThe t1 t2 = NP.insertString "to the" (t1 ^. term) (t2 ^. term)

--FIXME: As mentioned in issue #487, the following should be re-examined later,
--       as they may embody a deeper idea in some cases.

-- we might want to eventually restrict the use of these via
-- some kind of type system, which asserts that:
-- 1. t1 `for` t2 means that t1 is a view of part of the reason behind t2
-- 2. t1 `of_` t2 means that t1 is a view of part of the structure of t2

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "for" between
-- their terms. Plural case is @(phraseNP t1) "for" (pluralNP t2)@.
for :: (NamedIdea c, NamedIdea d) => c -> d -> NP
for t1 t2 = NP.insertString "for" (t1 ^. term) (t2 ^. term)

-- | Similar to 'for', but takes two functions that determine the 'titleCase'.
forTGen :: (NamedIdea c, Idea d) => (c -> NPStruct) -> (d -> NPStruct) -> c -> d -> NP
forTGen f1 f2 t1 t2 = nounPhrase''
  (phrase t1 :+: S "for" :+: phrase t2)
  (plural t1 :+: S "for" :+: phrase t2)
  CapFirst
  (Replace (f1 t1 :+: S "for" :+: f2 t2))

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "in" between
-- their terms. Plural case is @(phraseNP t1) "in" (pluralNP t2)@.
in_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
in_ t1 t2 = NP.insertString "in" (t1 ^. term) (t2 ^. term)

-- | Same as 'in_', except pluralNP case is @(pluralNP t1) "in" (phraseNP t2)@.
in_PS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
in_PS t1 t2 = NP.insertStringOp "in" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in a" between
-- their terms. Plural case is @(phraseNP t1) "in a" (pluralNP t2)@.
inA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inA t1 t2 = NP.insertString "in a" (t1 ^. term) (t2 ^. term)

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "is" between
-- their terms. Plural case is @(phraseNP t1) "is" (pluralNP t2)@.
is :: (NamedIdea c, NamedIdea d) => c -> d -> NP
is t1 t2 = NP.insertString "is" (t1 ^. term) (t2 ^. term)

-- | Prepends "the" to a 'NamedIdea'.
the :: (NamedIdea t) => t -> NP
the t = nounPhrase'' (S "the" :+: phrase t) (S "the" :+: plural t) CapFirst CapWords

-- | A customizable version of 'the'. The given function is applied to both singular and pluralNP cases.
theGen :: (t -> NPStruct) -> t -> NP
theGen f t = nounPhrase'' (S "the" :+: f t) (S "the" :+: f t) CapFirst CapWords

-- | Prepends "a" to a 'NamedIdea' (similar to 'the').
a_ :: (NamedIdea c) => c -> NP
a_ t = nounPhrase'' (S "a" :+: phrase t) (S "a" :+: plural t) CapFirst CapWords

-- | Customizable version of 'a'.
a_Gen :: (c -> NPStruct) -> c -> NP
a_Gen f t = nounPhrase'' (S "a" :+: f t) (S "a" :+: f t) CapFirst CapWords

-- | Combinator for combining two 'NamedIdeas's into a 'IdeaDict'.
-- Plural case only makes second term plural.
-- See 'compoundPhrase' for more on pluralNP behaviour.
-- /Does not preserve abbreviations/.
compoundNC :: (NamedIdea a, NamedIdea b) => a -> b -> IdeaDict
compoundNC t1 t2 = ncUID
  (t1 +++! t2) (compoundPhrase (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC' but both terms are pluralized for pluralNP case.
compoundNCPP :: (NamedIdea a, NamedIdea b) => a -> b -> IdeaDict
compoundNCPP t1 t2 = ncUID
  (t1 +++! t2) (compoundPhrase'' D.pluralNP D.pluralNP (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC', except pluralNP cases are customizable.
compoundNCGen :: (NamedIdea a, NamedIdea b) =>
  (NP -> NPStruct) -> (NP -> NPStruct) -> a -> b -> IdeaDict
compoundNCGen f1 f2 t1 t2 = ncUID
  (t1 +++! t2)
  (compoundPhrase'' f1 f2 (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC', except for pluralNP case, where first parameter gets pluralized while second one stays singular.
compoundNCPS :: IdeaDict -> IdeaDict -> IdeaDict
compoundNCPS = compoundNCGen D.pluralNP D.phraseNP

-- hack for Solution Characteristics Specification, calling upon pluralNP will pluralize
-- Characteristics as it is the end of the first term (solutionCharacteristic)
-- | Similar to 'compoundNC', but takes a function that is applied to the first term (eg. 'short' or 'plural').
compoundNCGenP :: (NamedIdea a, NamedIdea b) => (NP -> NPStruct) -> a -> b -> IdeaDict
compoundNCGenP f1 t1 t2 = ncUID
  (t1 +++! t2) (compoundPhrase''' f1 (t1 ^. term) (t2 ^. term))

-- FIXME: Same as above function
-- | Similar to 'compoundNCGenP' but sets first parameter function to plural.
compoundNCPSPP :: IdeaDict -> IdeaDict -> IdeaDict
compoundNCPSPP = compoundNCGenP D.pluralNP

-- | Helper function that combines a 'NamedIdea' and a 'NP' without any words in between.
-- Plural case is @(phraseNP t1) :+: (pluralNP t2)@.
combineNINP :: (NamedIdea c) => c -> NP -> NP
combineNINP t1 t2 = nounPhrase'' (phrase t1 :+: phraseNP t2) (phrase t1 :+: pluralNP t2) CapFirst CapWords

-- | Similar to 'combineNINP' but takes two 'NamedIdea's.
combineNINI :: (NamedIdea c, NamedIdea d) => c -> d -> NP
combineNINI t1 t2 = nounPhrase'' (phrase t1 :+: phrase t2) (phrase t1 :+: plural t2) CapFirst CapWords
