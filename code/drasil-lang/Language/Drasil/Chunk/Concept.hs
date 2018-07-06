{-# Language TypeFamilies #-}
module Language.Drasil.Chunk.Concept 
  ( ConceptChunk, dcc, dcc', dccWDS, dccWDS', cc, cc', ccs
  , cw, DefnAndDomain(DAD)
  , CommonConcept, ConceptInstance
  )where

import Language.Drasil.Classes (HasUID(uid), Idea, Definition(defn),
  ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.NamedIdea(mkIdea,nw)
import Language.Drasil.Chunk.CommonIdea (commonIdea)
import Language.Drasil.Spec (Sentence(S))
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Chunk.Concept.Core (ConceptChunk(ConDict), CommonConcept(ComConDict), 
  DefnAndDomain(DAD), ConceptInstance)


import Control.Lens ((^.))

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given an id, 
-- 'NounPhrase' ('NP') and definition (as String).
dcc i ter des = ConDict (mkIdea i ter Nothing) (DAD (S des) []) Nothing
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Identical to 'dcc', but adds an abbreviation (String)
dcc' :: String -> NP -> String -> String -> CommonConcept
dcc' i t d a = ComConDict (commonIdea i t a) (S d) []

-- | Similar to 'dcc', except the definition is a 'Sentence'
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = ConDict (mkIdea i t Nothing) (DAD d []) Nothing

-- | Similar to 'dcc', except the definition is a 'Sentence' and adds
-- an abbreviation (String)
dccWDS' :: String -> NP -> Sentence -> String -> CommonConcept
dccWDS' i t d a = ComConDict (commonIdea i t a) d []

-- | Constructor for 'ConceptChunk'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = ConDict (nw n) (DAD (S d) []) Nothing

-- | Same as cc, except definition is a 'Sentence'
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) (DAD d []) Nothing

-- | Constructor for 'ConceptChunk'. Allows explicit tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) (DAD d $ map (\x -> x ^. uid) l) Nothing

-- | For projecting out to the ConceptChunk data-type
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (DAD (c ^. defn) (c ^. cdom)) Nothing
