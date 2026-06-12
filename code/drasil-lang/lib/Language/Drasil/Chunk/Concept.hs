-- | Contains functions to create the concept related chunk types found in
-- "Language.Drasil.Chunk.Concept.Core".
module Language.Drasil.Chunk.Concept (
  -- * Concept Chunks
  -- ** From an idea ('IdeaDict')
  ConceptChunk, cncpt, cncpt', cncpt'', dcc, dccA, dccAWDS, cc', cw,
  -- ** From a 'ConceptChunk'
  ConceptInstance, cic
  ) where

import Control.Lens ((^.))

import Drasil.Database (HasUID(uid), nsUid)

import Language.Drasil.Classes (Idea, ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.Concept.Core (ConceptChunk(ConDict), ConceptInstance(ConInst))
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.Chunk.NamedIdea(mkIdea, nw, nc)
import Language.Drasil.NaturalLanguage.English.NounPhrase (NP, pn)
import Language.Drasil.ShortName (shortname')
import qualified Language.Drasil.Classes as D (defn)

--FIXME: Temporary ConceptDomain tag hacking to not break everything.

-- | Construct a 'ConceptChunk'.
cncpt :: Concept dom =>
  -- | The 'UID'.
  String ->
  -- The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence ->
  -- | The term's abbreviation, if one exists.
  Maybe String ->
  -- | The domain the 'term' belongs to.
  [dom] -> ConceptChunk
cncpt u trm defn mabbr = ConDict (mkIdea u trm mabbr) defn . map (^. uid)

-- | Construct a 'ConceptChunk'.
cncpt' ::
  -- | The 'UID'.
  String ->
  -- | The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence ->
  -- | The term's abbreviation, if one exists.
  Maybe String -> ConceptChunk
cncpt' u trm defn mabbr = cncpt u trm defn mabbr ([] :: [ConceptChunk])

-- | Construct a 'ConceptChunk'.
cncpt'' ::
  -- | The 'UID'.
  String ->
  -- | The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence -> ConceptChunk
cncpt'' u trm defn = cncpt' u trm defn Nothing

{-# DEPRECATED dccA, dccAWDS, dcc, cc', cw
  "Smart constructors allow externally-known chunk nesting; use one of `cncpt, cncpt', cncpt''` instead." #-}

-- | Smart constructor for creating a concept chunks with an abbreviation. Takes
-- a UID (String), a term (NounPhrase), a definition (String), and an
-- abbreviation (Maybe String).
dccA :: String -> NP -> String -> Maybe String -> ConceptChunk
dccA i ter des a = ConDict (mkIdea i ter a) (S des) []

dccAWDS :: String -> NP -> Sentence -> Maybe String -> ConceptChunk
dccAWDS i t d a = ConDict (mkIdea i t a) d []

-- | Smart constructor for creating concept chunks given a 'UID', 'NounPhrase'
-- ('NP') and definition (as a 'String').
dcc :: String -> NP -> String -> ConceptChunk
dcc i ter des = dccA i ter des Nothing

-- | Constructor for projecting an idea into a 'ConceptChunk'. Takes the
-- definition of the 'ConceptChunk' as a 'Sentence. Does not allow concept
-- domain tagging.
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) d []

-- | For projecting out to the 'ConceptChunk' data-type.
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (c ^. D.defn) (cdom c)

-- | Constructor for a 'ConceptInstance'. Takes in the Reference Address
-- ('String'), a definition ('Sentence'), a short name ('String'), and a domain
-- (for explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (nsUid "instance" $ icc ^. uid) icc u $ shortname' (S sn)
  where
    icc = cc (nc u $ pn sn) d [dom]
    cc n d' l = ConDict n d' $ map (^. uid) l
