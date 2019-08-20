-- | Utilities to get grab certain chunks (from Expr, Sentence, etc) by UID and
-- dereference the chunk it refers to.
module Database.Drasil.ChunkDB.GetChunk (ccss, ccss', combine, getIdeaDict, vars) where

import Language.Drasil

import Database.Drasil.ChunkDB (ChunkDB, defResolve, symbResolve, termResolve)

import Data.List (nub)

-- | Get a list of quantities (QuantityDict) from an equation in order to print
vars :: Expr -> ChunkDB -> [QuantityDict]
vars e m = map (symbResolve m) $ dep e

vars' :: Sentence -> ChunkDB -> [QuantityDict]
vars' a m = map (symbResolve m) $ sdep a

combine :: Sentence -> ChunkDB -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)

combine' :: Expr -> ChunkDB -> [DefinedQuantityDict]
combine' a m = zipWith dqdQd (vars a m) (concpt' a m)

ccss :: [Sentence] -> [Expr] -> ChunkDB -> [DefinedQuantityDict]
ccss s e c = nub $ concatMap (`combine` c) s ++ concatMap (`combine'` c) e

ccss' :: [Sentence] -> [Expr] -> ChunkDB -> [QuantityDict]
ccss' s e c = nub $ concatMap (`vars'` c) s ++ concatMap (`vars` c) e

concpt :: Sentence -> ChunkDB -> [ConceptChunk]
concpt a m = map (defResolve m) $ sdep a

concpt' :: Expr -> ChunkDB -> [ConceptChunk]
concpt' a m = map (defResolve m) $ dep a

getIdeaDict :: Sentence -> ChunkDB -> [IdeaDict]
getIdeaDict a m = map (termResolve m) $ shortdep a
