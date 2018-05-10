{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( cqs, DefinedQuantityDict
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom,DOM), Concept, HasSymbol(symbol),
  HasAttributes(attributes))
import Language.Drasil.Chunk.Concept (ConceptChunk,cw)
import qualified Language.Drasil.Chunk.Quantity as Q
import Language.Drasil.Chunk.Attribute.Core (Attributes)

import Control.Lens ((^.), makeLenses)

-- | DefinedQuantity = Concept + Quantity
-- not really the best as this duplicates |id|.  At least, it should...
data DefinedQuantityDict = DQD { _quant :: Q.QuantityDict
                               , _con :: ConceptChunk
                               , _attribs :: Attributes
                               }
  
makeLenses ''DefinedQuantityDict

-- but we pick it from the Quantity.
instance HasUID DefinedQuantityDict        where uid = quant . uid
instance Eq DefinedQuantityDict            where a == b = (a ^. uid) == (b ^. uid)
instance NamedIdea DefinedQuantityDict     where term = con . term
instance Idea DefinedQuantityDict          where getA (DQD a _ _) = getA a
instance Definition DefinedQuantityDict    where defn = con . defn
instance ConceptDomain DefinedQuantityDict where
  type DOM DefinedQuantityDict = ConceptChunk
  cdom = con . cdom
instance Concept DefinedQuantityDict       where
instance Q.HasSpace DefinedQuantityDict    where  typ = quant . Q.typ
instance HasSymbol DefinedQuantityDict     where symbol q st = symbol (q^.quant) st
instance Q.Quantity DefinedQuantityDict    where getUnit (DQD a _ _) = Q.getUnit a
instance HasAttributes DefinedQuantityDict where attributes = attribs

cqs :: (HasAttributes c, Q.Quantity c, Concept c, DOM c ~ ConceptChunk) => c -> DefinedQuantityDict
cqs c = DQD (Q.qw c) (cw c) (c ^. attributes) 
