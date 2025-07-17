-- | Utilities to get grab certain chunks (from 'Expr', 'Sentence', etc) by
-- 'UID' and dereference the chunk it refers to.
module Drasil.GetChunks (ccss, ccss', combine, vars, citeDB) where

import Language.Drasil
import Language.Drasil.Development
import Language.Drasil.ModelExpr.Development (meDep)

import Database.Drasil (ChunkDB, defResolve, symbResolve, citationTable)

import System.Drasil (System, systemdb)

import Control.Lens ((^.))
import Data.List (nub, sortBy)
import qualified Data.Map as M

-- | Gets a list of quantities ('QuantityDict') from an equation in order to print.
vars :: ModelExpr -> ChunkDB -> [DefinedQuantityDict]
vars e m = map (symbResolve m) $ meDep e

-- | Gets a list of quantities ('QuantityDict') from a 'Sentence' in order to print.
vars' :: Sentence -> ChunkDB -> [DefinedQuantityDict]
vars' a m = map (symbResolve m) $ sdep a

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
ccss' :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss' s e c = nub $ concatMap (`vars'` c) s ++ concatMap (`vars` c) e

-- | Gets a list of concepts ('ConceptChunk') from a 'Sentence' in order to print.
concpt :: Sentence -> ChunkDB -> [ConceptChunk]
concpt a m = map (defResolve m) $ sdep a

-- | Gets a list of concepts ('ConceptChunk') from an expression in order to print.
concpt' :: ModelExpr -> ChunkDB -> [ConceptChunk]
concpt' a m = map (defResolve m) $ meDep a

-- | Extract bibliography entries for a system.
citeDB :: System -> BibRef
citeDB si = sortBy compareAuthYearTitle $ map fst $ M.elems $ si ^. (systemdb . citationTable)
