-- | Utilities to get grab certain chunks (from Expr, Sentence, etc) by UID and
-- dereference the chunk it refers to.
module Database.Drasil.ChunkDB.GetChunk (ccss, combine,
  getIdeaDict, vars) where

import Language.Drasil

import Database.Drasil.ChunkDB (symbLookup, symbolTable,
 defLookup, defTable, ChunkDB, termLookup, termTable)

import Data.List(nub)

-- | Get a list of quantities (QuantityDict) from an equation in order to print
vars :: Expr -> ChunkDB -> [QuantityDict]
vars e m = map resolve $ dep e
  where resolve x = symbLookup x $ symbolTable m

concpt' :: Expr -> ChunkDB -> [ConceptChunk]
concpt' a m = map resolve $ dep a
  where resolve x = defLookup x $ defTable m

combine' :: Expr -> ChunkDB -> [DefinedQuantityDict]
combine' a m = zipWith dqdQd (vars a m) (concpt' a m)

ccss :: [Sentence] -> [Expr]-> ChunkDB -> [DefinedQuantityDict]
ccss s e c = nub $ concatMap (`combine` c) s ++ concatMap (`combine'` c) e

vars' :: Sentence -> ChunkDB -> [QuantityDict]
vars' a m = map resolve $ sdep a
  where resolve x = symbLookup x $ symbolTable m

concpt :: Sentence -> ChunkDB -> [ConceptChunk]
concpt a m = map resolve $ sdep a
  where resolve x = defLookup x $ defTable m

combine :: Sentence -> ChunkDB -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)

getIdeaDict :: Sentence -> ChunkDB -> [IdeaDict]
getIdeaDict a m = map resolve $ shortdep a
  where resolve x = termLookup x $ termTable m
