-- | Utilities to get grab certain chunks (from 'Expr', 'Sentence', etc) by 'UID' and
-- dereference the chunk it refers to.
module SysInfo.Drasil.GetChunk (ccss, ccss', combine, getIdeaDict, vars) where

import Language.Drasil
import Language.Drasil.Development
import Language.Drasil.ModelExpr.Development (meDep)

import Database.Drasil (ChunkDB, defResolve, symbResolve, termResolve)

import Data.List (nub)
import Data.Set (toList)

-- | Gets a list of quantities ('QuantityDict') from an equation in order to print.
vars :: ModelExpr -> ChunkDB -> [QuantityDict]
vars e m = map (symbResolve m) $ meDep e

-- | Gets a list of quantities ('QuantityDict') from a 'Sentence' in order to print.
vars' :: Sentence -> ChunkDB -> [QuantityDict]
vars' a m = map (symbResolve m) $ toList $ sdep a

-- | Combines the functions of 'vars' and 'concpt' to create a list of 'DefinedQuantityDict's from a 'Sentence'.
combine :: Sentence -> ChunkDB -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)

-- | Combines the functions of 'vars' and 'concpt' to create a list of 'DefinedQuantityDict's from an equation.
combine' :: ModelExpr -> ChunkDB -> [DefinedQuantityDict]
combine' a m = zipWith dqdQd (vars a m) (concpt' a m)

-- | Gets a list of defined quantities ('DefinedQuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss s e c = nub $ concatMap (`combine` c) s ++ concatMap (`combine'` c) e

-- | Gets a list of quantities ('QuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss' :: [Sentence] -> [ModelExpr] -> ChunkDB -> [QuantityDict]
ccss' s e c = nub $ concatMap (`vars'` c) s ++ concatMap (`vars` c) e

-- | Gets a list of concepts ('ConceptChunk') from a 'Sentence' in order to print.
concpt :: Sentence -> ChunkDB -> [ConceptChunk]
concpt a m = map (defResolve m) $ toList $ sdep a

-- | Gets a list of concepts ('ConceptChunk') from an expression in order to print.
concpt' :: ModelExpr -> ChunkDB -> [ConceptChunk]
concpt' a m = map (defResolve m) $ meDep a

-- | Gets a list of ideas ('IdeaDict') from a 'Sentence' in order to print.
getIdeaDict :: Sentence -> ChunkDB -> [IdeaDict]
getIdeaDict a m = map (termResolve m) $ toList $ shortdep a
