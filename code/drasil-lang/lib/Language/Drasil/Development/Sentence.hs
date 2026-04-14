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
  -- * from NPStruct to Sentence
  toSent,
  -- * Short Form (lowercase)
  short,
  -- * Introduce with Abbreviation
  introduceAbb, introduceAbbPlrl
) where

import Control.Lens ((^.))

import Drasil.Database (hide)

import Language.Drasil.Chunk.NamedIdea (NamedIdea(term), Idea)
import Language.Drasil.Sentence ((+:+), sParen, sentenceTerm,
  sentencePlural, sentenceShort)
import qualified Language.Drasil.Sentence as S
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase as NP
import Language.Drasil.NaturalLanguage.English.NounPhrase.Core (NPStruct(..))

-- | Translate from NPStruct to Sentence
toSent :: NPStruct -> S.Sentence
toSent (S s) = S.S s
toSent (s1 :-: s2) = toSent s1 S.:+: toSent s2 -- no space between noun phases
toSent (s1 :+: s2) = toSent s1 S.+:+ toSent s2 -- insert space between noun phrases
toSent (P p) = S.P p

-- | Get short form (if it exists), else get term of an 'Idea'.
-- Uses the UID of the 'Idea' in a 'Ch' Sentence constructor to get the short
-- form using getA. getA may return Nothing, in which case lookupS uses the
-- term, where lookupS is the main helper for looking up the short form of a
-- 'Ch' Sentence.
short :: Idea c => c -> S.Sentence
short = sentenceShort . hide

-- | Introduce a noun phrase and its (parenthesized) abbreviation.
introduceAbb :: Idea n => n -> S.Sentence
introduceAbb n = phrase n +:+ sParen (short n)

-- | Introduce a plural noun phrase and its (parenthesized) abbreviation.
introduceAbbPlrl :: Idea n => n -> S.Sentence
introduceAbbPlrl n = plural n +:+ sParen (short n)

-- | Helper function for getting the sentence case of a noun phrase from a
-- 'NamedIdea'.
atStart, atStart' :: NamedIdea n => n -> S.Sentence
-- | Singular sentence case.
atStart  n = toSent $ NP.atStartNP (n ^. term)
-- | Plural sentence case.
atStart' n = toSent $ NP.atStartNP' (n ^. term)

-- | Helper function for getting the title case of a noun phrase from a
-- 'NamedIdea'.
titleize, titleize' :: NamedIdea n => n -> S.Sentence
-- | Singular title case.
titleize  n = toSent $ NP.titleizeNP (n ^. term)
-- | Plural title case.
titleize' n = toSent $ NP.titleizeNP' (n ^. term)

-- | Helper for getting the phrase from a 'NamedIdea' using it's UID.
phrase :: Idea n => n -> S.Sentence
phrase = sentenceTerm . hide

-- | Helper for getting the plural of a phrase from a 'NamedIdea'.
plural :: Idea n => n -> S.Sentence
plural = sentencePlural . hide

-- | Helper for getting the possesive cases from the term of a 'NamedIdea'.
phrasePoss, pluralPoss :: Idea n => n -> S.Sentence
-- | Singular possesive function
phrasePoss a = phrase a S.:+: S.S "'s"
-- | Plural possesive function
pluralPoss a = plural a S.:+: S.S "'"
