-- | Contains functions to create the concept related chunk types found in "Language.Drasil.Chunk.Concept.Core".
module Language.Drasil.Chunk.Concept (
  -- * Concept Chunks
  -- ** From an idea ('IdeaDict')
  Conception, dcc, dccWDS, cc, cc', ccs, cw,
  -- ** From a 'Conception'
  ConceptInstance, cic
  ) where

import Language.Drasil.Classes (Idea, Definition(defn), ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.Concept.Core (Conception(ConDict), ConceptInstance(ConInst))
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.Chunk.NamedIdea(mkIdea,nw, nc)
import Language.Drasil.NounPhrase (NP, pn)
import Language.Drasil.ShortName (shortname')
import Language.Drasil.UID (HasUID(uid))

import Control.Lens ((^.))

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> Conception 
-- | Smart constructor for creating concept chunks given a 'UID', 
-- 'NounPhrase' ('NP') and definition (as a 'String').
dcc i ter des = ConDict (mkIdea i ter Nothing) (S des) []
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Similar to 'dcc', except the definition takes a 'Sentence'.
dccWDS :: String -> NP -> Sentence -> Conception
dccWDS i t d = ConDict (mkIdea i t Nothing) d []

-- | Constructor for projecting an idea into a 'Conception'. Takes the definition of the 
-- 'Conception' as a 'String'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> Conception
cc n d = ConDict (nw n) (S d) []

-- | Same as 'cc', except definition is a 'Sentence'.
cc' :: Idea c => c -> Sentence -> Conception
cc' n d = ConDict (nw n) d []

-- | Similar to 'cc'', but allows explicit domain tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> Conception --Explicit tagging
ccs n d l = ConDict (nw n) d $ map (^. uid) l

-- | For projecting out to the 'Conception' data-type.
cw :: Concept c => c -> Conception
cw c = ConDict (nw c) (c ^. defn) (cdom c)

-- | Constructor for a 'ConceptInstance'. Takes in the 
-- Reference Address ('String'), a definition ('Sentence'), 
-- a short name ('String'), and a domain (for explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (ccs (nc u $ pn sn) d [dom]) u $ shortname' (S sn)
