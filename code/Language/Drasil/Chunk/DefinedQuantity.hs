{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( dqd, dqd', dqdEL, DefinedQuantityDict, dqdWr
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), Concept, HasSymbol(symbol),
  HasAttributes(attributes), HasSpace(typ))
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import qualified Language.Drasil.Chunk.Quantity as Q
import Language.Drasil.Unit (UnitDefn)
import Language.Drasil.Symbol (Symbol, Stage)
import Language.Drasil.Space (Space)
import Language.Drasil.Chunk.Attribute.Core (Attributes)

import Control.Lens ((^.), makeLenses, view)

-- | DefinedQuantity = Concept + Quantity
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _unit :: Maybe UnitDefn
                               , _attribs :: Attributes
                               }
  
makeLenses ''DefinedQuantityDict

instance HasUID        DefinedQuantityDict where uid = con . uid
instance Eq            DefinedQuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance NamedIdea     DefinedQuantityDict where term = con . term
instance Idea          DefinedQuantityDict where getA (DQD a _ _ _ _) = getA a
instance Definition    DefinedQuantityDict where defn = con . defn
instance ConceptDomain DefinedQuantityDict where
  type DOM DefinedQuantityDict = ConceptChunk
  cdom = con . cdom
instance Concept       DefinedQuantityDict where
instance Q.HasSpace    DefinedQuantityDict where typ = spa
instance HasSymbol     DefinedQuantityDict where symbol = view symb
instance HasAttributes DefinedQuantityDict where attributes = attribs
instance Q.Quantity    DefinedQuantityDict where getUnit = view unit -- needed for use with qw wrapper


-- For when the symbol is constant through stages
dqd :: ConceptChunk -> Symbol -> Space -> Attributes -> DefinedQuantityDict
dqd c s sp atts = DQD c (\_ -> s) sp Nothing atts

-- For when the symbol changes depending on the stage
dqd' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Attributes -> DefinedQuantityDict
dqd' c symbs sp atts = DQD c symbs sp Nothing atts

-- Same as dqd, but passes an empty list as the Attibutes
dqdEL :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDict
dqdEL c s sp = DQD c (\_ -> s) sp Nothing []

dqdWr :: (Concept c, HasAttributes c, Q.HasSpace c, HasSymbol c, DOM c ~ ConceptChunk) => c -> DefinedQuantityDict
dqdWr c = DQD (cw c) (symbol c) (c ^. typ) Nothing (c ^. attributes)