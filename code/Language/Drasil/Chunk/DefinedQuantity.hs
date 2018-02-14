{-# LANGUAGE GADTs, Rank2Types, TemplateHaskell #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( cqs, DefinedQuantityDict
  ) where

import Control.Lens ((^.), makeLenses)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import qualified Language.Drasil.Chunk.Quantity as Q

import Prelude hiding (id)

-- | DefinedQuantity = Concept + Quantity
-- not really the best as this duplicates |id|.  At least, it should...
data DefinedQuantityDict = DQD { _quant :: Q.QuantityDict, _con :: ConceptChunk }
  
makeLenses ''DefinedQuantityDict

-- but we pick it crom the Quantity.
instance Chunk DefinedQuantityDict where
  id = quant . id
  
instance Eq DefinedQuantityDict where
  a == b = (a ^. id) == (b ^. id)
  
instance Ord DefinedQuantityDict where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((Q.getSymb Equational a) ^. symbol) ((Q.getSymb Equational b) ^. symbol)
  
instance NamedIdea DefinedQuantityDict where
  term = con . term

instance Idea DefinedQuantityDict where
  getA (DQD a _) = getA a
  
instance Concept DefinedQuantityDict where
  defn = con . defn
  cdom = con . cdom
  
instance Q.Quantity DefinedQuantityDict where
  getSymb s (DQD a _) = Q.getSymb s a
  getUnit (DQD a _) = Q.getUnit a
  typ = quant . Q.typ
  getStagedS (DQD a _) = Q.getStagedS a

cqs :: (Q.Quantity c, Concept c) => c -> DefinedQuantityDict
cqs c = DQD (Q.qw c) (cw c)
