{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( cqs, cqs', DefinedQuantityDict
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom,DOM), Concept, HasSymbol(symbol),
  HasAttributes(attributes))
import Language.Drasil.Chunk.Concept (ConceptChunk)
import qualified Language.Drasil.Chunk.Quantity as Q

import Language.Drasil.Symbol (Symbol, Stage)
import Language.Drasil.Space (Space)
import Language.Drasil.Chunk.Attribute.Core (Attributes)

import Control.Lens ((^.), makeLenses, view)

-- | DefinedQuantity = Concept + Quantity
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _attribs :: Attributes
                               }
  
makeLenses ''DefinedQuantityDict

instance HasUID        DefinedQuantityDict where uid = con . uid
instance Eq            DefinedQuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance NamedIdea     DefinedQuantityDict where term = con . term
instance Idea          DefinedQuantityDict where getA (DQD a _ _ _) = getA a
instance Definition    DefinedQuantityDict where defn = con . defn
instance ConceptDomain DefinedQuantityDict where
  type DOM DefinedQuantityDict = ConceptChunk
  cdom = con . cdom
instance Concept       DefinedQuantityDict where
instance Q.HasSpace    DefinedQuantityDict where typ = spa
instance HasSymbol     DefinedQuantityDict where symbol = view symb
instance HasAttributes DefinedQuantityDict where attributes = attribs

cqs :: ConceptChunk -> Symbol -> Space -> Attributes -> DefinedQuantityDict
cqs c s sp atts = DQD c (\_ -> s) sp atts

cqs' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Attributes -> DefinedQuantityDict
cqs' c symbs sp atts = DQD c symbs sp atts
