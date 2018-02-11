{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Concept 
  ( Concept(..), ConceptChunk, dcc, dcc', dccWDS, dccWDS', cc, cc', ccs
  , CWrapper, cw
  )where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.CommonIdea (commonIdea)

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Spec

import Prelude hiding (id)
import Language.Drasil.NounPhrase

-- | Concepts are 'Idea's with definitions
class Idea c => Concept c where
  -- | defn provides (a 'Lens' to) the definition for a chunk
  defn :: Simple Lens c Sentence
  -- | cdom provides (a 'Lens' to) the concept domain tags for a chunk
  cdom :: Simple Lens c [CWrapper] 
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- === DATA TYPES === --
--- ConceptChunk ---  

-- | The ConceptChunk datatype is a Concept
data ConceptChunk where
  -- CC takes an 'Idea', a definition, and domain tags.
  CC :: Idea c => c -> Sentence -> [CWrapper] -> ConceptChunk 
  -- [CWrapper] is a list of the ConceptDomain(s) as Concepts themselves.
  -- It is not exported, see 'cc' and 'ccs' for the exported constructors.
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConceptChunk where
  id = nl id
instance NamedIdea ConceptChunk where
  term = nl term
instance Idea ConceptChunk where
  getA (CC n _ _) = getA n
instance Concept ConceptChunk where
  defn f (CC n d cd) = fmap (\x -> CC n x cd) (f d)
  cdom f (CC n d cd) = fmap (\x -> CC n d x) (f cd)
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens ConceptChunk a
nl l f (CC n d cd) = fmap (\x -> CC (set l x n) d cd) (f (n ^. l))

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given an id, 
-- 'NounPhrase' ('NP') and definition (as String).
dcc i ter des = CC (nc i ter) (S des) ([] :: [CWrapper])
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Identical to 'dcc', but adds an abbreviation (String)
dcc' :: String -> NP -> String -> String -> ConceptChunk
dcc' i t d a = CC (commonIdea i t a) (S d) ([] :: [CWrapper])

-- | Similar to 'dcc', except the definition is a 'Sentence'
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = CC (nc i t) d ([] :: [CWrapper])

-- | Similar to 'dcc', except the definition is a 'Sentence' and adds
-- an abbreviation (String)
dccWDS' :: String -> NP -> Sentence -> String -> ConceptChunk
dccWDS' i t d a = CC (commonIdea i t a) d ([] :: [CWrapper])

-- | Constructor for 'ConceptChunk'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = CC n (S d) ([] :: [CWrapper])

-- | Same as cc, except definition is a 'Sentence'
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = CC n (d) ([] :: [CWrapper])

-- | Constructor for 'ConceptChunk'. Allows explicit tagging.
ccs :: Idea c => c -> Sentence -> [CWrapper] -> ConceptChunk --Explicit tagging
ccs = CC

{- Concept Wrapper -}
-- | Wrapper for Concepts
data CWrapper where
  CW :: (Concept c) => c -> CWrapper
  
instance Chunk CWrapper where
  id = clens id
  
instance NamedIdea CWrapper where
  term = clens term
instance Idea CWrapper where
  getA (CW a) = getA a
  
instance Concept CWrapper where
  defn = clens defn
  cdom = clens cdom

-- | CWrapper constructor. Takes the Concept to be wrapped.
-- Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
cw :: Concept c => c -> CWrapper
cw = CW

clens :: (forall c. (Concept c) => 
  Simple Lens c a) -> Simple Lens CWrapper a
clens l f (CW a) = fmap (\x -> CW (set l x a)) (f (a ^. l))

instance Eq CWrapper where
 a == b = (a ^. id) == (b ^. id)
  
