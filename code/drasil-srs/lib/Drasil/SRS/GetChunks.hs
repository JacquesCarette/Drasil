-- | Utilities to get grab certain chunks (from 'Expr', 'Sentence', etc) by
-- 'UID' and dereference the chunk it refers to.
module Drasil.SRS.GetChunks (
  ccss, ccss', vars
) where

import qualified Data.Set as Set

import Language.Drasil
import Language.Drasil.Development (sdep)
import Language.Drasil.ModelExpr.Development (meDep)
import Drasil.Database (ChunkDB, findOrErr)

-- | Gets a list of quantities ('DefinedQuantityDict') from an equation in order to print.
vars :: ModelExpr -> ChunkDB -> [DefinedQuantityDict]
vars e m = map (`findOrErr` m) $ meDep e

-- | Gets a list of quantities ('DefinedQuantityDict') from a 'Sentence' in order to print.
vars' :: Sentence -> ChunkDB -> [DefinedQuantityDict]
vars' a m = map (`findOrErr` m) $ Set.toList (sdep a)

-- | Gets a list of defined quantities ('DefinedQuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss s e c = concatMap (`vars'` c) s ++ concatMap (`vars` c) e

-- | Gets a list of quantities ('DefinedQuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss' :: [Sentence] -> [ModelExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss' s e c = concatMap (`vars'` c) s ++ concatMap (`vars` c) e
