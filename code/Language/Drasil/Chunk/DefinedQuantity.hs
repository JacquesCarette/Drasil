{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( cqs, cqs', cqsEL, DefinedQuantityDict, cqsWr
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), Concept, HasSymbol(symbol),
  HasAttributes(attributes), HasSpace(typ))
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
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
instance Q.Quantity    DefinedQuantityDict where getUnit con = Nothing -- needed for use with qw wrapper


-- For when the symbol is constant through stages
cqs :: ConceptChunk -> Symbol -> Space -> Attributes -> DefinedQuantityDict
cqs c s sp atts = DQD c (\_ -> s) sp atts

-- For when the symbol changes depending on the stage
cqs' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Attributes -> DefinedQuantityDict
cqs' c symbs sp atts = DQD c symbs sp atts

-- Same as cqs, but passes an empty list as the Attibutes
cqsEL :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDict
cqsEL c s sp = DQD c (\_ -> s) sp []

cqsWr :: (Concept c, HasAttributes c, Q.HasSpace c, HasSymbol c, DOM c ~ ConceptChunk) => c -> DefinedQuantityDict
cqsWr c = DQD (cw c) (symbol c) (c ^. typ) (c ^. attributes)
