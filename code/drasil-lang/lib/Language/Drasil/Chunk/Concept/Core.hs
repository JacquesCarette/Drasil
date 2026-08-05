{-# Language TemplateHaskell #-}
-- | Define concept-related chunks. A concept is usually something that has
-- a term, definition, and comes from some domain of knowledge.
module Language.Drasil.Chunk.Concept.Core(
  -- * Concept-related Datatypes
  ConceptChunk(ConDict)
  , sDom
) where

import Control.Lens (makeLenses, (^.),)

import Drasil.Database (UID, HasUID(..), declareHasChunkRefs, Generically(..))

import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom))
import Language.Drasil.NaturalLanguage.English.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)

-- | Check if something has one domain. Throws an error if there is more than one.
sDom :: [UID] -> UID
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

-- | The 'ConceptChunk' datatype records a concept that contains a unique id ('UID'),
-- a term ('NP'), a definition ('Sentence'), an optional abbreviation ('Maybe String'),
-- and an associated domain of knowledge (['UID']).
--
-- Ex. The concept of "Accuracy" may be defined as the quality or state of being correct or precise.
data ConceptChunk = ConDict { _uu :: UID -- ^ The 'UID' of the concept.
                            , _np :: NP -- ^ The term for the concept.
                            , mabbr :: Maybe String -- ^ The optional abbreviation for the concept.
                            , _defn' :: Sentence -- ^ The definition of the concept.
                            , cdom' :: [UID] -- ^ Domain of the concept.
                            }
makeLenses ''ConceptChunk
declareHasChunkRefs ''ConceptChunk

-- | Equal if 'UID's are equal.
instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds 'UID' of the 'ConceptChunk'.
instance HasUID        ConceptChunk where uid = uu
-- | Finds term ('NP') of the 'ConceptChunk'.
instance NamedIdea     ConceptChunk where term = np
-- | Finds the abbreviation of the 'ConceptChunk'.
instance Idea          ConceptChunk where getA = mabbr
-- | Finds definition of a 'ConceptChunk'.
instance Definition    ConceptChunk where defn = defn'
-- | Finds the domain of 'UID's of a 'ConceptChunk'.
instance ConceptDomain ConceptChunk where cdom = cdom'
