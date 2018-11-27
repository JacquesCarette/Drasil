-- Various helpers for building Sentences from other bits.
-- Really ought to be moved out to (likely) docLang, but is here for now.
module Language.Drasil.Development.Sentence where

import Control.Lens ((^.))

import Language.Drasil.Classes (NamedIdea(term), HasUID(uid), Idea)
import Language.Drasil.Chunk.NamedIdea (short)
import Language.Drasil.Sentence ((+:+), Sentence((:+:), S), sParen, sentenceTerm,
  sentencePlural)
import qualified Language.Drasil.NounPhrase as NP

-- | Helper for common pattern of introducing the title-case version of a 
-- noun phrase (from an Idea)
-- followed by its abbreviation in parentheses.
introduceAbb :: Idea n => n -> Sentence
introduceAbb n = NP.titleizeNP (n ^. term) +:+ sParen (short n)

-- | Helper function for getting the sentence case of a noun phrase from a 
-- NamedIdea.
at_start, at_start' :: NamedIdea n => n -> Sentence
-- | Singular sentence case.
at_start  n = NP.at_startNP (n ^. term)
-- | Plural sentence case.
at_start' n = NP.at_startNP' (n ^. term)

-- | Helper function for getting the title case of a noun phrase from a 
-- NamedIdea.
titleize, titleize' :: NamedIdea n => n -> Sentence
-- | Singular title case.
titleize  n = NP.titleizeNP (n ^. term)
-- | Plural title case.
titleize' n = NP.titleizeNP' (n ^. term)

-- | Helper for getting the phrase from a NamedIdea.
phrase :: (HasUID n, NamedIdea n) => n -> Sentence
phrase n = sentenceTerm (n ^. uid) --NP.phrase (n ^. term)

-- | Helper for getting the plural of a phrase from a NamedIdea
plural :: (HasUID n, NamedIdea n) => n -> Sentence
plural n = sentencePlural (n ^. uid)
--plural n = NP.plural (n ^. term)

phrase's, plural's :: NamedIdea n => n -> Sentence
-- | Singular possesive function
phrase's a = phrase a :+: S "'s"
-- | Plural possesive function
plural's a = plural a :+: S "'"
