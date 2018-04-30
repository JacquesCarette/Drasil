{-# LANGUAGE TemplateHaskell #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( cqs, DefinedQuantityDict
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term))
import Language.Drasil.Chunk.NamedIdea (Idea(..))
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm (HasSymbol(symbol))
import qualified Language.Drasil.Chunk.Quantity as Q

import Control.Lens ((^.), makeLenses)

-- | DefinedQuantity = Concept + Quantity
-- not really the best as this duplicates |id|.  At least, it should...
data DefinedQuantityDict = DQD { _quant :: Q.QuantityDict, _con :: ConceptChunk }
  
makeLenses ''DefinedQuantityDict

-- but we pick it from the Quantity.
instance HasUID DefinedQuantityDict        where uid = quant . uid
instance Eq DefinedQuantityDict            where a == b = (a ^. uid) == (b ^. uid)
instance NamedIdea DefinedQuantityDict     where term = con . term
instance Idea DefinedQuantityDict          where getA (DQD a _) = getA a
instance Definition DefinedQuantityDict    where defn = con . defn
instance ConceptDomain DefinedQuantityDict where cdom = con . cdom
instance Concept DefinedQuantityDict       where
instance Q.HasSpace DefinedQuantityDict    where  typ = quant . Q.typ
instance HasSymbol DefinedQuantityDict     where symbol q st = symbol (q^.quant) st
instance Q.Quantity DefinedQuantityDict    where getUnit (DQD a _) = Q.getUnit a

cqs :: (Q.Quantity c, Concept c) => c -> DefinedQuantityDict
cqs c = DQD (Q.qw c) (cw c)
