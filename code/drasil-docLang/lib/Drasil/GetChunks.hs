-- | Utilities to get grab certain chunks (from 'Expr', 'Sentence', etc) by
-- 'UID' and dereference the chunk it refers to.
module Drasil.GetChunks (
  ccss, ccss', combine, vars,
  lookupCitations, resolveBibliography
) where

import Data.List (nub, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Language.Drasil
import Language.Drasil.Development (sdep)
import Language.Drasil.ModelExpr.Development (meDep)
import Drasil.Database (ChunkDB, findOrErr, find, UID)
import Drasil.Database.SearchTools (defResolve', DomDefn(definition))

-- | Gets a list of quantities ('DefinedQuantityDict') from an equation in order to print.
vars :: ModelExpr -> ChunkDB -> [DefinedQuantityDict]
vars e m = map (`findOrErr` m) $ meDep e

-- | Gets a list of quantities ('DefinedQuantityDict') from a 'Sentence' in order to print.
vars' :: Sentence -> ChunkDB -> [DefinedQuantityDict]
vars' a m = map (`findOrErr` m) $ S.toList (sdep a)

-- | Combines the functions of 'vars' and 'concpt' to create a list of 'DefinedQuantityDict's from a 'Sentence'.
combine :: Sentence -> ChunkDB -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)

-- | Combines the functions of 'vars' and 'concpt' to create a list of 'DefinedQuantityDict's from an equation.
combine' :: ModelExpr -> ChunkDB -> [DefinedQuantityDict]
combine' a m = zipWith dqdQd (vars a m) (concpt' a m)

-- | Gets a list of defined quantities ('DefinedQuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss s e c = nub $ concatMap (`combine` c) s ++ concatMap (`combine'` c) e

-- | Gets a list of quantities ('DefinedQuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss' :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss' s e c = nub $ concatMap (`vars'` c) s ++ concatMap (`vars` c) e

-- | Gets a list of concepts ('ConceptChunk') from a 'Sentence' in order to print.
concpt :: Sentence -> ChunkDB -> [Sentence]
concpt a m = map (definition . defResolve' m) $ S.toList (sdep a)

-- | Gets a list of concepts ('ConceptChunk') from an expression in order to print.
concpt' :: ModelExpr -> ChunkDB -> [Sentence]
concpt' a m = map (definition . defResolve' m) $ meDep a

-- | Look up citation chunks from the database using their UIDs.
lookupCitations :: ChunkDB -> [UID] -> [Citation]
lookupCitations db uids = mapMaybe (`find` db) (nub uids)

resolveBibliography :: ChunkDB -> S.Set UID -> [Citation]
resolveBibliography db uids = sortBy compareAuthYearTitle cites
  where
    cites = mapMaybe (`find` db) (S.toList uids)
