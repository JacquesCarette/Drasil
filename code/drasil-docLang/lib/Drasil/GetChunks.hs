-- | Utilities to get grab certain chunks (from 'Expr', 'Sentence', etc) by
-- 'UID' and dereference the chunk it refers to.
module Drasil.GetChunks (
  ccss, ccss', combine, vars,
  resolveBibliography
) where

import Data.List (nub, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

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
vars' a m = map (`findOrErr` m) $ Set.toList (sdep a)

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
concpt a m = map (definition . defResolve' m) $ Set.toList (sdep a)

-- | Gets a list of concepts ('ConceptChunk') from an expression in order to print.
concpt' :: ModelExpr -> ChunkDB -> [Sentence]
concpt' a m = map (definition . defResolve' m) $ meDep a

-- | Given a 'ChunkDB' and a set of 'UID's, looks up the corresponding
-- 'Citation's and returns them sorted by author, year, and title.
--
-- FIXME: This function assumes that all 'UID's in the set correspond to
-- 'Citation's in the database. If a 'UID' does not correspond to a 'Citation',
-- it is simply ignored. This should rather rely on a set of 'UIDRef Citation's.
resolveBibliography :: ChunkDB -> Set.Set UID -> [Citation]
resolveBibliography db uids = sortBy compareAuthYearTitle cites
  where
    cites = mapMaybe (`find` db) (Set.toList uids)
