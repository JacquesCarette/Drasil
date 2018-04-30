{-# Language TemplateHaskell, FlexibleInstances #-}
module Language.Drasil.Chunk.Concept 
  ( ConceptChunk, dcc, dcc', dccWDS, dccWDS', cc, cc', ccs
  , cw, DefnAndDomain(DAD)
  , Definition(defn), ConceptDomain(cdom), Concept
  , CommonConcept
  )where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA))
import Language.Drasil.Chunk.NamedIdea(IdeaDict,mkIdea,nw)
import Language.Drasil.Chunk.CommonIdea (CI,CommonIdea(abrv),commonIdea)
import Language.Drasil.Spec
import Language.Drasil.NounPhrase

import Control.Lens (Simple, Lens, (^.), makeLenses, view)

-- === DATA TYPES === --
--- ConceptChunk ---  

data DefnAndDomain a = DAD { _defn' :: Sentence, _cdom' :: [a]}
makeLenses ''DefnAndDomain

-- | The ConceptChunk datatype is a Concept
-- ConDict is not exported, nor are _idea and _dad
data ConceptChunk = ConDict { _idea :: IdeaDict, _dad :: DefnAndDomain ConceptChunk }
makeLenses ''ConceptChunk

class Definition c where
  -- | defn provides (a 'Lens' to) the definition for a chunk
  defn :: Simple Lens c Sentence

class ConceptDomain c where
  -- | cdom provides (a 'Lens' to) the concept domain tags for a chunk
  cdom :: Simple Lens c [ConceptChunk] 
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- | Concepts are 'Idea's with definitions and domains
class (Idea c, Definition c, ConceptDomain c) => Concept c where

instance Definition    (DefnAndDomain a) where defn = defn'
instance ConceptDomain (DefnAndDomain ConceptChunk) where cdom = cdom'

instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        ConceptChunk where uid = idea . uid
instance NamedIdea     ConceptChunk where term = idea . term
instance Idea          ConceptChunk where getA = getA . view idea
instance Definition    ConceptChunk where defn = dad . defn'
instance ConceptDomain ConceptChunk where cdom = dad . cdom'
instance Concept       ConceptChunk where
 
data CommonConcept = ComConDict { _comm :: CI, _def :: Sentence, _dom :: [ConceptChunk]}
makeLenses ''CommonConcept

instance Eq            CommonConcept where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        CommonConcept where uid = comm . uid
instance NamedIdea     CommonConcept where term = comm . term
instance Idea          CommonConcept where getA = getA . view comm
instance Definition    CommonConcept where defn = def
instance CommonIdea    CommonConcept where abrv = abrv . view comm
instance ConceptDomain CommonConcept where cdom = dom
instance Concept       CommonConcept where

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given an id, 
-- 'NounPhrase' ('NP') and definition (as String).
dcc i ter des = ConDict (mkIdea i ter Nothing) (DAD (S des) [])
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Identical to 'dcc', but adds an abbreviation (String)
dcc' :: String -> NP -> String -> String -> CommonConcept
dcc' i t d a = ComConDict (commonIdea i t a) (S d) []

-- | Similar to 'dcc', except the definition is a 'Sentence'
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = ConDict (mkIdea i t Nothing) (DAD d [])

-- | Similar to 'dcc', except the definition is a 'Sentence' and adds
-- an abbreviation (String)
dccWDS' :: String -> NP -> Sentence -> String -> CommonConcept
dccWDS' i t d a = ComConDict (commonIdea i t a) d []

-- | Constructor for 'ConceptChunk'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = ConDict (nw n) (DAD (S d) [])

-- | Same as cc, except definition is a 'Sentence'
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) (DAD d [])

-- | Constructor for 'ConceptChunk'. Allows explicit tagging.
ccs :: Idea c => c -> Sentence -> [ConceptChunk] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) (DAD d l)

-- | For projecting out to the ConceptChunk data-type
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (DAD (c ^. defn) (c ^. cdom))
