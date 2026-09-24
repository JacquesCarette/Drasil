-- | Defines various NounPhrase-level combinators. These hold more information
-- than those of the Sentence level but may not necessarily have a conceptual link.
-- See the [Wiki](https://github.com/JacquesCarette/Drasil/wiki/Combinator-Documentation)
-- for more information and details about the naming process for combinators.
-- A summary of the function naming scheme is as follows:
--
--    * Combinators that conflict with haskell-native functions have an underscore appended.
--    * Default plural case for combinators will be first term singular, second term plural.
--    * @P@ and @S@ denote the plural case of the combinator when it does not follow the above default.
--    * @Gen@ denotes the general function case.
--
-- This module should be used as a qualified import (usually as @NP@),
-- as many function names clash with those in Concepts.hs and Sentence.hs.
module Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators (
  -- * General Combinator Helper Functions
  insertString, prependString, insertStringOp, insertStringGen,
  -- * Prepositions
  -- ** \"The\" Combinators
  the, theGen,
  -- ** \"A\" Combinators
  a_, a_Gen,
  -- * Conjunctions
  -- ** \"And\" Combinators
  and_, and_PS, and_Gen, andThe,
  -- ** \"The\" Combinators
  ofThe, ofThePS, ofTheGen, inThe, inThePS, inTheGen,
  the_ofThe, the_ofThePS, the_ofTheGen,
  -- ** \"For\" Combinators
  for, forPS, forGen,
  -- ** \"Of\" Combinators
  of_, of_PS, of_Gen,
  -- ** Other Combinators
  with, parensNP,
  -- re-exports
  npS, npP, (.+.)
) where

import Language.Drasil.NaturalLanguage.English.NounPhrase
    ( NPG,
      CapitalizationRuleG(CapWords, CapFirst, CapNothing),
      NounPhrase(phraseNP, pluralNP),
      nounPhrase'', surroundNPStruct,
      npS, npP, (.+.), NPStructG)

--Maybe move these to a separate Drasil.NounPhrase section
-- | Helper function that places a 'String' in between two 'NP's. Plural case is
-- @(phraseNP t1) :+: S s :+: (pluralNP t2)@.
insertString :: String -> NPG a -> NPG a -> NPG a
insertString str t1 t2 = nounPhrase'' (phraseNP t1 .+. npS str .+. phraseNP t2) (phraseNP t1 .+. npS str .+. pluralNP t2) CapFirst CapWords

-- | Helper function that places a 'String' in between two 'NP's. Plural case is
-- @(pluralNP t1) :+: S s :+: (phraseNP t2)@, i.e. opposite of 'insertString'
insertStringOp :: String -> NPG a -> NPG a -> NPG a
insertStringOp str t1 t2 = nounPhrase'' (phraseNP t1 .+. npS str .+. phraseNP t2) (pluralNP t1 .+. npS str .+. phraseNP t2) CapFirst CapWords

-- | Helper function that places a 'String' in between two 'NP's. Plural case is
-- given by two generic functions.
insertStringGen :: String -> (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
insertStringGen str f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 .+. npS str .+. phraseNP t2) (f1 t1 .+. npS str .+. f2 t2) CapFirst CapWords

-- | Helper function that prepends a 'String' to a 'NP'.
prependString :: String -> NPG a -> NPG a
prependString str t1 = nounPhrase'' (npS str .+. phraseNP t1) (npS str .+. pluralNP t1) CapFirst CapWords

-- | Prepends a 'String' to an 'NP' and adjust the plural
prependStringGen :: String -> (NPG a -> NPStructG a) -> NPG a -> NPG a
prependStringGen str f t = nounPhrase'' (npS str .+. phraseNP t) (npS str .+. f t) CapFirst CapWords

-- surrounding something makes it no capitalizatble at all
surround :: String -> String -> NPG a -> NPG a
surround l r t = nounPhrase'' (surroundNPStruct l r (phraseNP t)) (surroundNPStruct l r (pluralNP t)) CapNothing CapNothing

{-
-- | Helper function that places a 'Sentence' in between two 'NP's. Plural case is
-- @(phraseNP t1) :+: s :+: (pluralNP t2)@.
insertSent :: Sentence -> NP -> NP -> NP
insertSent s t1 t2 = nounPhrase'' (phraseNP t1 :+: s :+: phraseNP t2) (phraseNP t1 :+: s :+: pluralNP t2) CapFirst CapWords

-- | Helper function that prepends a 'Sentence' to a 'NP'.
prependSent :: Sentence -> NP -> NP
prependSent s t1 = nounPhrase'' (s :+: phraseNP t1) (s :+: pluralNP t1) CapFirst CapWords
-}

-- | Prepends "the" to a 'NP'.
the :: NPG a -> NPG a
the = prependString "the"
-- | Similar to 'the', but accepts a function that determines the plural case.
theGen :: (NPG a -> NPStructG a) -> NPG a -> NPG a
theGen = prependStringGen "the"

-- | Prepends "a" to a 'NP'.
a_ :: NPG a -> NPG a
a_ = prependString "a"
-- | Similar to 'a', but accepts a function that determines the plural case.
a_Gen :: (NPG a -> NPStructG a) -> NPG a -> NPG a
a_Gen = prependStringGen "a"

-- | Inserts "of the" between two 'NP's. Plural case is @(phraseNP t1) :+: "of the" :+: (pluralNP t2)@.
ofThe :: NPG a -> NPG a -> NPG a
ofThe = insertString "of the"
-- | Similar to 'ofThe', but the plural case is now @(pluralNP t1) `ofThe` (phraseNP t2)@.
--
ofThePS :: NPG a -> NPG a -> NPG a
ofThePS = insertStringOp "of the"
-- | Similar to 'ofThe', but accepts two functions for the plural case.
ofTheGen :: (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
ofTheGen = insertStringGen "of the"

-- | Inserts "in the" between two 'NP's. Plural case is @(phraseNP t1) :+: "in the" :+: (pluralNP t2)@.
inThe :: NPG a -> NPG a -> NPG a
inThe = insertString "in the"
-- | Similar to 'ofThe', but the plural case is now @(pluralNP t1) `inThe` (phraseNP t2)@.
inThePS :: NPG a -> NPG a -> NPG a
inThePS = insertStringOp "in the"
-- | Similar to 'ofThe', but accepts two functions for the plural case.
inTheGen :: (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
inTheGen = insertStringGen "in the"

-- | Prepends "the" and inserts "of the". Plural case is @"the" :+: (phraseNP t1) :+: "of the" :+: (pluralNP t2)@.
the_ofThe :: NPG a -> NPG a -> NPG a
the_ofThe t1 t2 = the t1 `ofThe` t2
-- | Similar to 'the_ofThe', but the plural case is now @ S "the" :+: (pluralNP t1) `ofThe` (phraseNP t2)@.
the_ofThePS :: NPG a -> NPG a -> NPG a
the_ofThePS t1 t2 = the t1 `ofThePS` t2
-- | Similar to 'the_ofThe'', but takes two functions for the plural case.
the_ofTheGen :: (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
the_ofTheGen f1 f2 t1 = ofTheGen f1 f2 (the t1)

-- | Inserts "for" between two 'NP's. Plural case is @(phraseNP t1) :+: "for" :+: (pluralNP t2)@.
for :: NPG a -> NPG a -> NPG a
for = insertString "for"
-- | Same as 'for', but plural case is now @(pluralNP t1) `for` (phraseNP t2)@.
forPS :: NPG a -> NPG a -> NPG a
forPS = insertStringOp "for"
-- | Same as 'for'', but takes two functions for the plural case.
forGen :: (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
forGen = insertStringGen "for"

-- | Inserts "of" between two 'NP's. Plural case is @(phraseNP t1) :+: "of" :+: (pluralNP t2)@.
of_ :: NPG a -> NPG a -> NPG a
of_ = insertString "of"
-- | Same as 'of_', but plural case is now @(pluralNP t1) `of_` (phraseNP t2)@.
of_PS :: NPG a -> NPG a -> NPG a
of_PS = insertStringOp "of"
-- | Same as 'of_', but takes two functions for the plural case.
of_Gen :: (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
of_Gen = insertStringGen "of"

-- | Inserts "with" between two 'NP's. Plural case is @(phraseNP t1) :+: "with" :+: (pluralNP t2)@.
with :: NPG a -> NPG a -> NPG a
with = insertString "with"

-- | Inserts "and" between two 'NP's. Plural case is @(phraseNP t1) :+: "and" :+: (pluralNP t2)@.
and_ :: NPG a -> NPG a -> NPG a
and_ = insertString "and"
-- | Same as 'and_', but plural case is now @(pluralNP t1) `and_` (phraseNP t2)@.
and_PS :: NPG a -> NPG a -> NPG a
and_PS = insertStringOp "and"
-- | Same as 'and_', but takes two functions for the plural case.
and_Gen :: (NPG a -> NPStructG a) -> (NPG a -> NPStructG a) -> NPG a -> NPG a -> NPG a
and_Gen = insertStringGen "and"

-- | Inserts "and the" between two 'NP's. Plural case is @(phraseNP t1) :+: "and the" :+: (pluralNP t2)@.
andThe :: NPG a -> NPG a -> NPG a
andThe = insertString "and the"

-- | Puts parentheses around a word; to be used as a kind of postfix qualifier
parensNP :: NPG a -> NPG a
parensNP = surround "(" ")"
