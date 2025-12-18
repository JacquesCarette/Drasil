-- | Contains functions to create the concept related chunk types found in "Language.Drasil.Chunk.Concept.Core".
module Language.Drasil.Chunk.Concept (
  -- * Concept Chunks
  -- ** From an idea ('IdeaDict')
  ConceptChunk, dcc, dccA, dccAWDS, dccWDS, cc', ccs, cw,
  -- ** From a 'ConceptChunk'
  ConceptInstance, cic
  ) where

import Control.Lens ((^.))

import Drasil.Database (HasUID(uid), nsUid)

import Language.Drasil.Classes (Idea, Definition(defn), ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.Concept.Core (ConceptChunk(ConDict), ConceptInstance(ConInst))
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.Chunk.NamedIdea(mkIdea,nw, nc)
import Language.Drasil.NounPhrase (NP, pn)
import Language.Drasil.ShortName (shortname')

--FIXME: Temporary ConceptDomain tag hacking to not break everything.

-- | Smart constructor for creating a concept chunks with an abbreviation
-- Takes a UID (String), a term (NounPhrase), a definition (String), and an abbreviation (Maybe String).
dccA :: String -> NP -> String -> Maybe String -> ConceptChunk
dccA i ter des a = ConDict (mkIdea i ter a) (S des) []

dccAWDS :: String -> NP -> Sentence -> Maybe String -> ConceptChunk
dccAWDS i t d a = ConDict (mkIdea i t a) d []

dcc :: String -> NP -> String -> ConceptChunk
-- | Smart constructor for creating concept chunks given a 'UID',
-- 'NounPhrase' ('NP') and definition (as a 'String').
dcc i ter des = dccA i ter des Nothing
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Similar to 'dcc', except the definition takes a 'Sentence'.
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = dccAWDS i t d Nothing

-- | Constructor for projecting an idea into a 'ConceptChunk'. Takes the definition of the
-- 'ConceptChunk' as a 'Sentence. Does not allow concept domain tagging.
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) d []

-- | Similar to 'cc'', but allows explicit domain tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) d $ map (^. uid) l

-- | For projecting out to the 'ConceptChunk' data-type.
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (c ^. defn) (cdom c)

-- | Constructor for a 'ConceptInstance'. Takes in the
-- Reference Address ('String'), a definition ('Sentence'),
-- a short name ('String'), and a domain (for explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (nsUid "instance" $ icc ^. uid) icc u $ shortname' (S sn)
  where icc = ccs (nc u $ pn sn) d [dom]
