-- | Noun phrases are used to hold terms with knowledge of proper capitalization and pluralization.
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Drasil.NounPhrase.Core (
  module Language.Drasil.NounPhrase.Types
) where

import Drasil.Database (HasChunkRefs(..), UID)

import Language.Drasil.NounPhrase.Types
import qualified Language.Drasil.Sentence as Sent
import Language.Drasil.Sentence (Sentence, sentenceRefs)

import qualified Data.Set as Set

-- | Gather the chunk references mentioned within an 'NP'.
instance HasChunkRefs NP where
    chunkRefs = npRefs
    {-# INLINABLE chunkRefs #-}

-- | Collect references contained in a noun phrase.
npRefs :: NP -> Set.Set UID
npRefs (ProperNoun _ _)         = Set.empty
npRefs (CommonNoun _ _ cRule)   = capRuleRefs cRule
npRefs (Phrase sing plural c1 c2) =
  npStructRefs sing `Set.union`
  npStructRefs plural `Set.union`
  capRuleRefs c1 `Set.union`
  capRuleRefs c2
{-# INLINABLE npRefs #-}

-- | Extract references embedded in capitalization rules (only 'Replace').
capRuleRefs :: CapitalizationRule -> Set.Set UID
capRuleRefs CapFirst     = Set.empty
capRuleRefs CapWords     = Set.empty
capRuleRefs CapNothing   = Set.empty
capRuleRefs (Replace s)  = npStructRefs s
{-# INLINABLE capRuleRefs #-}

-- | Convert a noun-phrase structure into a sentence and reuse 'sentenceRefs'.
npStructRefs :: NPStruct -> Set.Set UID
npStructRefs = sentenceRefs . npStructToSentence
{-# INLINABLE npStructRefs #-}

-- | Translate 'NPStruct' into a 'Sentence' so that sentence helpers can traverse it.
npStructToSentence :: NPStruct -> Sentence
npStructToSentence (S s)       = Sent.S s
npStructToSentence (a :-: b)   = npStructToSentence a Sent.:+: npStructToSentence b
npStructToSentence (a :+: b)   = npStructToSentence a Sent.+:+ npStructToSentence b
npStructToSentence (P sym)     = Sent.P sym
