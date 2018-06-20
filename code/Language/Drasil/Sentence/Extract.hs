module Language.Drasil.Sentence.Extract(sdep, vars',snames, concpt) where

import Data.List (nub)
import Control.Lens ((^.))
import Language.Drasil.Expr (Expr(..), RealInterval(..))
import Language.Drasil.ChunkDB (HasSymbolTable, symbLookup, symbolTable, HasDefinitionTable,
 defLookup, defTable)
import Language.Drasil.Chunk.Code (CodeChunk, codevar)
import Language.Drasil.Chunk.Quantity (QuantityDict)
import Language.Drasil.Chunk.Concept (ConceptChunk)
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Spec(Sentence(..))
import Language.Drasil.Expr.Extract(names)

-- | Generic traverse of all positions that could lead to names from sentences
snames   :: Sentence -> [String]
snames (Ch a)       = [a]
snames (Sy _)       = []
snames (S _)        = []
snames (Sp _)       = []
snames (P _)        = []
snames (Ref _ _ _)  = []
snames ((:+:) a b)  = (snames a) ++ (snames b)
snames (Quote a)    = snames a
snames (E a)        = names a
snames (EmptyS)     = []
-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
sdep :: Sentence -> [String]
sdep = nub . snames

vars' :: (HasSymbolTable s) => Sentence -> s -> [QuantityDict]
vars' a m = map resolve $ sdep a
  where resolve x = symbLookup x $ m ^. symbolTable

concpt :: (HasDefinitionTable s) => Sentence -> s -> [ConceptChunk]
concpt a m = map resolve $ sdep a
  where resolve x = defLookup x $ m ^. defTable

combine :: (HasSymbolTable s, HasDefinitionTable s) => Sentence -> s -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)

