module Language.Drasil.Sentence.Extract(sdep, snames) where

import Data.List (nub)
import Control.Lens ((^.))
import Language.Drasil.Expr (Expr(..), RealInterval(..))
import Language.Drasil.ChunkDB (HasSymbolTable, symbLookup, symbolTable, HasDefinitionTable,
 defLookup, defTable)
import Language.Drasil.Chunk.Quantity (QuantityDict)
import Language.Drasil.Chunk.Concept (ConceptChunk)
import Language.Drasil.Spec(Sentence(..))
import Language.Drasil.Expr.Extract(names)
import Language.Drasil.Chunk.DefinedQuantity
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
