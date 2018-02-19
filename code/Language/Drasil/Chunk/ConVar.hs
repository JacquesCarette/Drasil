{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.ConVar
  ( ConVar(CV) , cv , cvR
  , cvRs -- Temporary identification for the the creation of a quantity with a certain type of unit.  Will eventually change to cvR.
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Space (HasSpace(typ))
import Language.Drasil.Chunk.SymbolForm (Stage,HasSymbol(symbol))
import Language.Drasil.Chunk.Quantity (Quantity(getUnit))

import Control.Lens ((^.),makeLenses)

import Language.Drasil.Symbol
import Language.Drasil.Space

import Prelude hiding (id)

-- | ConVar is a 'Concept' as well as a 'Quantity'. 
-- It adds a 'Space' and 'Symbol' to an existing 'ConceptChunk'.
data ConVar = CV { _con :: ConceptChunk
                 , _symb :: Stage -> Symbol
                 , _spa :: Space }
makeLenses ''ConVar
                     
instance Eq ConVar where c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConVar where id = con . id
instance NamedIdea ConVar where term = con . term
instance Idea ConVar where getA (CV c _ _) = getA c
instance Definition ConVar where defn = con . defn
instance ConceptDomain ConVar where cdom = con . cdom
instance HasSymbol ConVar where symbol st c = (c ^. symb) st
instance HasSpace ConVar where typ = spa
instance Concept ConVar where
instance Quantity ConVar where getUnit _ = Nothing

-- | Constructor for 'ConVar' with explicit 'Space'
cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv c s = CV c (\_ -> s)

--FIXME: Remove this hack
-- | Constructor for 'ConVar' with implied 'Language.Drasil.Space.Real' 'Space'.
cvR :: ConceptChunk -> Symbol -> ConVar
cvR c s = CV c (\_ -> s) Real

cvRs :: ConceptChunk -> Symbol -> Space -> ConVar
cvRs c s p = CV c (\_ -> s) p
