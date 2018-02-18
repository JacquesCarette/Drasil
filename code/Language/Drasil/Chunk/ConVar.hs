module Language.Drasil.Chunk.ConVar
  ( ConVar(CV) , cv , cvR
  , cvRs -- Temporary identification for the the creation of a quantity with a certain type of unit.  Will eventually change to cvR.
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.SymbolForm (StagedSymbolChunk,ssc')
import Language.Drasil.Chunk.Concept

import Control.Lens (Lens', (^.))

import Language.Drasil.Symbol
import Language.Drasil.Space

import Prelude hiding (id)

-- | ConVar is a 'Concept' as well as a 'Language.Drasil.Chunk.Quantity'. 
-- It adds a 'Space' and 'Symbol' to an existing 'ConceptChunk'.
data ConVar = CV { _con :: ConceptChunk
                 , _symb :: StagedSymbolChunk
                 , _typ :: Space }
                     
instance Eq ConVar where c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConVar where id = cvl . id
instance NamedIdea ConVar where term = cvl . term
instance Idea ConVar where getA (CV c _ _) = getA c
instance Definition ConVar where defn = cvl . defn
instance ConceptDomain ConVar where cdom = cvl . cdom
instance Concept ConVar where

cvl :: Lens' ConVar ConceptChunk
cvl f (CV c s t) = fmap (\x -> CV x s t) (f c)

-- | Constructor for 'ConVar' with explicit 'Space'
cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv c s = CV c (ssc' (c ^. id) s)

--FIXME: Remove this hack
-- | Constructor for 'ConVar' with implied 'Language.Drasil.Space.Real' 'Space'.
cvR :: ConceptChunk -> Symbol -> ConVar
cvR c s = CV c (ssc' (c ^. id) s) Real

cvRs :: ConceptChunk -> Symbol -> Space -> ConVar
cvRs c s p = CV c (ssc' (c ^. id) s) p
