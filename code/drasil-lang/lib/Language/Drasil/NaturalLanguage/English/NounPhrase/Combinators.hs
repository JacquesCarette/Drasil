-- | Defines various NounPhrase-level combinators.
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
  NPStruct((:+:), S, P)
) where

import Language.Drasil.NaturalLanguage.English.NounPhrase
    ( NP,
      CapitalizationRule(CapWords, CapFirst, CapNothing),
      NounPhrase(phraseNP, pluralNP),
      nounPhrase'', surroundNPStruct)
import Language.Drasil.NaturalLanguage.English.NounPhrase.Core (NPStruct((:+:),S,P))

-- | Helper function that places a 'String' in between two 'NP's.
insertString :: String -> NP -> NP -> NP
insertString s t1 t2 = nounPhrase'' (phraseNP t1 :+: S s :+: phraseNP t2) (phraseNP t1 :+: S s :+: pluralNP t2) CapFirst CapWords

-- | Helper function that places a 'String' in between two 'NP's (opposite plural).
insertStringOp :: String -> NP -> NP -> NP
insertStringOp s t1 t2 = nounPhrase'' (phraseNP t1 :+: S s :+: phraseNP t2) (pluralNP t1 :+: S s :+: phraseNP t2) CapFirst CapWords

-- | Helper function that places a 'String' in between two 'NP's with generic plural functions.
insertStringGen :: String -> (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
insertStringGen s f1 f2 t1 t2 = nounPhrase'' (phraseNP t1 :+: S s :+: phraseNP t2) (f1 t1 :+: S s :+: f2 t2) CapFirst CapWords

-- | Helper function that prepends a 'String' to a 'NP'.
prependString :: String -> NP -> NP
prependString s t1 = nounPhrase'' (S s :+: phraseNP t1) (S s :+: pluralNP t1) CapFirst CapWords

-- | Prepends a 'String' to an 'NP' and adjust the plural
prependStringGen :: String -> (NP -> NPStruct) -> NP -> NP
prependStringGen s f t = nounPhrase'' (S s :+: phraseNP t) (S s :+: f t) CapFirst CapWords

-- surrounding something makes it no capitalizatble at all
surround :: String -> String -> NP -> NP
surround l r t = nounPhrase'' (surroundNPStruct l r (phraseNP t)) (surroundNPStruct l r (pluralNP t)) CapNothing CapNothing

-- | Prepends "the" to a 'NP'.
the :: NP -> NP
the = prependString "the"
-- | Similar to 'the', but accepts a function that determines the plural case.
theGen :: (NP -> NPStruct) -> NP -> NP
theGen = prependStringGen "the"

-- | Prepends "a" to a 'NP'.
a_ :: NP -> NP
a_ = prependString "a"
-- | Similar to 'a', but accepts a function that determines the plural case.
a_Gen :: (NP -> NPStruct) -> NP -> NP
a_Gen = prependStringGen "a"

-- | Inserts "of the" between two 'NP's.
ofThe :: NP -> NP -> NP
ofThe = insertString "of the"
-- | Similar to 'ofThe', but the plural case is opposite.
ofThePS :: NP -> NP -> NP
ofThePS = insertStringOp "of the"
-- | Similar to 'ofThe', but accepts two functions for the plural case.
ofTheGen :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
ofTheGen = insertStringGen "of the"

-- | Inserts "in the" between two 'NP's.
inThe :: NP -> NP -> NP
inThe = insertString "in the"
-- | Similar to 'inThe', but the plural case is opposite.
inThePS :: NP -> NP -> NP
inThePS = insertStringOp "in the"
-- | Similar to 'inThe', but accepts two functions for the plural case.
inTheGen :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
inTheGen = insertStringGen "in the"

-- | Prepends "the" and inserts "of the".
the_ofThe :: NP -> NP -> NP
the_ofThe t1 t2 = the t1 `ofThe` t2
-- | Similar to 'the_ofThe', but the plural case is opposite.
the_ofThePS :: NP -> NP -> NP
the_ofThePS t1 t2 = the t1 `ofThePS` t2
-- | Similar to 'the_ofThe', but takes two functions for the plural case.
the_ofTheGen :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
the_ofTheGen f1 f2 t1 = ofTheGen f1 f2 (the t1)

-- | Inserts "for" between two 'NP's.
for :: NP -> NP -> NP
for = insertString "for"
-- | Same as 'for', but plural case is opposite.
forPS :: NP -> NP -> NP
forPS = insertStringOp "for"
-- | Same as 'for', but takes two functions for the plural case.
forGen :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
forGen = insertStringGen "for"

-- | Inserts "of" between two 'NP's.
of_ :: NP -> NP -> NP
of_ = insertString "of"
-- | Same as 'of_', but plural case is opposite.
of_PS :: NP -> NP -> NP
of_PS = insertStringOp "of"
-- | Same as 'of_', but takes two functions for the plural case.
of_Gen :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
of_Gen = insertStringGen "of"

-- | Inserts "with" between two 'NP's.
with :: NP -> NP -> NP
with = insertString "with"

-- | Inserts "and" between two 'NP's.
and_ :: NP -> NP -> NP
and_ = insertString "and"
-- | Same as 'and_', but plural case is opposite.
and_PS :: NP -> NP -> NP
and_PS = insertStringOp "and"
-- | Same as 'and_', but takes two functions for the plural case.
and_Gen :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
and_Gen = insertStringGen "and"

-- | Inserts "and the" between two 'NP's.
andThe :: NP -> NP -> NP
andThe = insertString "and the"

-- | Puts parentheses around a word.
parensNP :: NP -> NP
parensNP = surround "(" ")"
