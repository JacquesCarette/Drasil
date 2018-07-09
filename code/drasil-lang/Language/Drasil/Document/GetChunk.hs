module Language.Drasil.Document.GetChunk (vars, combine', vars', combine, ccss)where

import Control.Lens ((^.))
import Data.List(transpose, head, tail, nub)

import Language.Drasil.Document
import Language.Drasil.Expr
import Language.Drasil.Spec

import Language.Drasil.Expr.Extract(dep)
import Language.Drasil.Sentence.Extract(sdep)

import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.References
import Language.Drasil.ChunkDB (HasSymbolTable, symbLookup, symbolTable, HasDefinitionTable,
 defLookup, defTable, ChunkDB)
import Language.Drasil.Chunk.Concept(ConceptChunk)
import Language.Drasil.Chunk.DefinedQuantity(DefinedQuantityDict, dqdQd)
import Language.Drasil.Classes (NamedIdea(term),
  ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), Definition(defn))


-- | Get a list of quantities (QuantityDict) from an equation in order to print
vars :: (HasSymbolTable s) => Expr -> s -> [QuantityDict]
vars e m = map resolve $ dep e
  where resolve x = symbLookup x $ m ^. symbolTable

concpt' :: (HasDefinitionTable s) => Expr -> s -> [ConceptChunk]
concpt' a m = map resolve $ dep a
  where resolve x = defLookup x $ m ^. defTable

combine' :: (HasSymbolTable s, HasDefinitionTable s) => Expr -> s -> [DefinedQuantityDict]
combine' a m = zipWith dqdQd (vars a m) (concpt' a m)

ccss :: [Sentence] -> [Expr]-> ChunkDB -> [DefinedQuantityDict]
ccss s e c = nub $ (concatMap (\x -> combine x c) s) ++ (concatMap (\x -> combine' x c) e)

vars' :: (HasSymbolTable s) => Sentence -> s -> [QuantityDict]
vars' a m = map resolve $ sdep a
  where resolve x = symbLookup x $ m ^. symbolTable

concpt :: (HasDefinitionTable s) => Sentence -> s -> [ConceptChunk]
concpt a m = map resolve $ sdep a
  where resolve x = defLookup x $ m ^. defTable

combine :: (HasSymbolTable s, HasDefinitionTable s) => Sentence -> s -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)