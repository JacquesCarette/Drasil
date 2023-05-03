{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Various helpers for building Sentences from other bits.
-- Really ought to be moved out to (likely) docLang, but is here for now.
module Language.Drasil.Development.Sentence (
  -- * All Lowercase
  phrase, plural, phrasePoss, pluralPoss,
  -- * Capitalize First Word
  atStart, atStart',
  -- * Capitalize All Words
  titleize, titleize',
  -- * Short Form (lowercase)
  short, introduceAbb) where

import Control.Lens ((^.))

import Language.Drasil.Classes (NamedIdea(term), Idea)
import Language.Drasil.Sentence ((+:+), Sentence((:+:), S), sParen, sentenceTerm,
  sentencePlural, sentenceShort)
import qualified Language.Drasil.NounPhrase as NP
import Language.Drasil.UID (HasUID(..))

-- | Get short form (if it exists), else get term of an 'Idea'.
short :: Idea c => c -> Sentence
short c = sentenceShort (c ^. uid)

-- | Helper for common pattern of introducing the title-case version of a 
-- noun phrase (from an Idea)
-- followed by its abbreviation in parentheses.
introduceAbb :: Idea n => n -> Sentence
introduceAbb n = NP.titleizeNP (n ^. term) +:+ sParen (short n)

-- | Helper function for getting the sentence case of a noun phrase from a 
-- 'NamedIdea'.
atStart, atStart' :: NamedIdea n => n -> Sentence
-- | Singular sentence case.
atStart  n = NP.atStartNP (n ^. term)
-- | Plural sentence case.
atStart' n = NP.atStartNP' (n ^. term)

-- | Helper function for getting the title case of a noun phrase from a 
-- 'NamedIdea'.
titleize, titleize' :: NamedIdea n => n -> Sentence
-- | Singular title case.
titleize  n = NP.titleizeNP (n ^. term)
-- | Plural title case.
titleize' n = NP.titleizeNP' (n ^. term)

-- | Helper for getting the phrase from a 'NamedIdea' using it's UID.
phrase :: NamedIdea n => n -> Sentence
phrase n = sentenceTerm (n ^. uid)

-- | Helper for getting the plural of a phrase from a 'NamedIdea'.
plural :: NamedIdea n => n -> Sentence
plural n = sentencePlural (n ^. uid)
--plural n = NP.plural (n ^. term)

-- | Helper for getting the possesive cases from the term of a 'NamedIdea'.
phrasePoss, pluralPoss :: NamedIdea n => n -> Sentence
-- | Singular possesive function
phrasePoss a = phrase a :+: S "'s"
-- | Plural possesive function
pluralPoss a = plural a :+: S "'"
