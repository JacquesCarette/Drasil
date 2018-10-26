-- Various helpers for building Sentences from other bits.
-- Really ought to be moved out to (likely) docLang, but is here for now.
module Language.Drasil.Development.Sentence where

import Control.Lens ((^.))

import Language.Drasil.Classes (NamedIdea(term), Idea)
import Language.Drasil.Chunk.NamedIdea (short)
import Language.Drasil.Spec ((+:+), Sentence((:+:), S), sParen)
import qualified Language.Drasil.NounPhrase as NP

-- | Helper for common pattern of introducing the title-case version of a 
-- noun phrase (from an Idea)
-- followed by its abbreviation in parentheses.
introduceAbb :: Idea n => n -> Sentence
introduceAbb n = NP.titleize (n ^. term) +:+ sParen (short n)

-- | Helper function for getting the sentence case of a noun phrase from a 
-- NamedIdea.
at_start, at_start' :: NamedIdea n => n -> Sentence
-- | Singular sentence case.
at_start  n = NP.at_start (n ^. term)
-- | Plural sentence case.
at_start' n = NP.at_start' (n ^. term)

-- | Helper function for getting the title case of a noun phrase from a 
-- NamedIdea.
titleize, titleize' :: NamedIdea n => n -> Sentence
-- | Singular title case.
titleize  n = NP.titleize (n ^. term)
-- | Plural title case.
titleize' n = NP.titleize' (n ^. term)

-- | Helper for getting the phrase from a NamedIdea.
phrase :: NamedIdea n => n -> Sentence
phrase n = NP.phrase (n ^. term)

-- | Helper for getting the plural of a phrase from a NamedIdea
plural :: NamedIdea n => n -> Sentence
plural n = NP.plural (n ^. term)

phrase's, plural's :: NamedIdea n => n -> Sentence
-- | Singular possesive function
phrase's a = phrase a :+: S "'s"
-- | Plural possesive function
plural's a = plural a :+: S "'"
