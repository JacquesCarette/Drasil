{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.ConVar
  ( ConVar(CV), cv, makeCV
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom,DOM),Concept)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Space (HasSpace(typ))
import Language.Drasil.Chunk.SymbolForm (Stage,HasSymbol(symbol))
import Language.Drasil.Chunk.Quantity (Quantity(getUnit))
import Language.Drasil.Symbol
import Language.Drasil.Space

import Control.Lens ((^.),makeLenses)

-- | ConVar is a 'Concept' as well as a 'Quantity'. 
-- It adds a 'Space' and 'Symbol' to an existing 'ConceptChunk'.
data ConVar = CV { _con :: ConceptChunk
                 , _symb :: Stage -> Symbol
                 , _spa :: Space }
makeLenses ''ConVar
                     
instance Eq ConVar            where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID ConVar        where uid = con . uid
instance NamedIdea ConVar     where term = con . term
instance Idea ConVar          where getA (CV c _ _) = getA c
instance Definition ConVar    where defn = con . defn
instance ConceptDomain ConVar where
  type DOM ConVar = ConceptChunk
  cdom = con . cdom
instance HasSymbol ConVar     where symbol c = (c ^. symb)
instance HasSpace ConVar      where typ = spa
instance Concept ConVar       where
instance Quantity ConVar      where getUnit _ = Nothing

-- | Constructor for 'ConVar' with explicit 'Space'
cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv c s = CV c (\_ -> s)

-- | Make a ConVar out of a combined |Concept| + |Quantity|
makeCV :: (Quantity c, Concept c, DOM c ~ ConceptChunk) => c -> ConVar
makeCV c = CV (cw c) (symbol c) (c ^. typ)
