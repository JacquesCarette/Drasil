{-# Language TypeFamilies #-}
module Language.Drasil.Chunk.Concept 
  ( ConceptChunk, dcc, dcc', dccWDS, dccWDS', cc, cc', ccs, cic, cw
  , CommonConcept, ConceptInstance
  ) where

import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Idea, Definition(defn), ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.CommonIdea (commonIdea)
import Language.Drasil.Chunk.Concept.Core (ConceptChunk(ConDict),
  ConceptInstance(ConInst), CommonConcept(ComConDict))
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.Chunk.NamedIdea(mkIdea,nw, nc)
import Language.Drasil.NounPhrase (NP, pn)
import Language.Drasil.ShortName (shortname')

import Control.Lens ((^.))

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given an id, 
-- 'NounPhrase' ('NP') and definition (as String).
dcc i ter des = ConDict (mkIdea i ter Nothing) (S des) []
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Identical to 'dcc', but adds an abbreviation (String)
dcc' :: String -> NP -> String -> String -> CommonConcept
dcc' i t d a = ComConDict (commonIdea i t a []) (S d) []

-- | Similar to 'dcc', except the definition is a 'Sentence'
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = ConDict (mkIdea i t Nothing) d []

-- | Similar to 'dcc', except the definition is a 'Sentence' and adds
-- an abbreviation (String)
dccWDS' :: String -> NP -> Sentence -> String -> CommonConcept
dccWDS' i t d a = ComConDict (commonIdea i t a []) d []

-- | Constructor for 'ConceptChunk'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = ConDict (nw n) (S d) []

-- | Same as cc, except definition is a 'Sentence'
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) d []

-- | Constructor for 'ConceptChunk'. Allows explicit tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) d $ map (^. uid) l

-- | For projecting out to the ConceptChunk data-type
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (c ^. defn) (cdom c)

cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (ccs (nc u $ pn sn) d [dom]) $ shortname' sn
