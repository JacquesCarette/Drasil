{-# Language TemplateHaskell, FlexibleInstances #-}
module Language.Drasil.Chunk.Concept 
  ( ConceptChunk, dcc, dcc', dccWDS, dccWDS', cc, cc', ccs
  , cw, DefnAndDomain(DAD)
  , Definition(defn), ConceptDomain(cdom), Concept
  )where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea

import Control.Lens (Simple, Lens, (^.), makeLenses, view)

import Language.Drasil.Spec

import Prelude hiding (id)
import Language.Drasil.NounPhrase

-- === DATA TYPES === --
--- ConceptChunk ---  

data DefnAndDomain a = DAD { _defn' :: Sentence, _cdom' :: [a]}
makeLenses ''DefnAndDomain

-- | The ConceptChunk datatype is a Concept
-- CC is not exported, nor are _idea and _dad
data ConceptChunk = CC { _idea :: IdeaDict, _dad :: DefnAndDomain ConceptChunk }
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

instance Eq            ConceptChunk where c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk         ConceptChunk where id = idea . id
instance NamedIdea     ConceptChunk where term = idea . term
instance Idea          ConceptChunk where getA = getA . view idea
instance Definition    ConceptChunk where defn = dad . defn'
instance ConceptDomain ConceptChunk where cdom = dad . cdom'
instance Concept       ConceptChunk where
 
--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given an id, 
-- 'NounPhrase' ('NP') and definition (as String).
dcc i ter des = CC (mkIdea i ter Nothing) (DAD (S des) [])
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Identical to 'dcc', but adds an abbreviation (String)
dcc' :: String -> NP -> String -> String -> ConceptChunk
dcc' i t d a = CC (mkIdea i t (Just a)) (DAD (S d) [])

-- | Similar to 'dcc', except the definition is a 'Sentence'
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = CC (mkIdea i t Nothing) (DAD d [])

-- | Similar to 'dcc', except the definition is a 'Sentence' and adds
-- an abbreviation (String)
dccWDS' :: String -> NP -> Sentence -> String -> ConceptChunk
dccWDS' i t d a = CC (mkIdea i t (Just a)) (DAD d [])

-- | Constructor for 'ConceptChunk'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = CC (nw n) (DAD (S d) [])

-- | Same as cc, except definition is a 'Sentence'
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = CC (nw n) (DAD d [])

-- | Constructor for 'ConceptChunk'. Allows explicit tagging.
ccs :: Idea c => c -> Sentence -> [ConceptChunk] -> ConceptChunk --Explicit tagging
ccs n d l = CC (nw n) (DAD d l)

-- | For projecting out to the ConceptChunk data-type
cw :: Concept c => c -> ConceptChunk
cw c = CC (nw c) (DAD (c ^. defn) (c ^. cdom))
