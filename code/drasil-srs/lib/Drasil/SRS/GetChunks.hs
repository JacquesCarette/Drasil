-- | Utilities to get grab certain chunks (from 'Expr', 'Sentence', etc) by
-- 'UID' and dereference the chunk it refers to.
module Drasil.SRS.GetChunks (
  resolveAllVars, vars
) where

import qualified Data.Set as Set

import Language.Drasil
import Language.Drasil.Development (sdep)
import Language.Drasil.ModelExpr.Development (meDep)
import Drasil.Database (ChunkDB, findOrErr)

-- | Extract and resolve all referenced 'DefinedQuantityDict's in a 'ModelExpr'.
vars :: ModelExpr -> ChunkDB -> [DefinedQuantityDict]
vars e m = map (`findOrErr` m) $ meDep e

-- | Extract and resolve all referenced 'DefinedQuantityDict's in a 'Sentence'.
vars' :: Sentence -> ChunkDB -> [DefinedQuantityDict]
vars' a m = map (`findOrErr` m) $ Set.toList (sdep a)

-- | Extract and resolve all references to 'DefinedQuantityDict's in a list of
-- 'Sentence's and a list of 'ModelExpr's.
resolveAllVars :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
resolveAllVars s e c = concatMap (`vars'` c) s ++ concatMap (`vars` c) e
