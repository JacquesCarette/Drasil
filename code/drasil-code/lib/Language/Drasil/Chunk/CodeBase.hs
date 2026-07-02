module Language.Drasil.Chunk.CodeBase (
  codevars, codevars', varResolve
) where

import Drasil.Database (ChunkDB, findOrErr, UID)
import Language.Drasil

import Drasil.Code.CodeExpr.Development
import Drasil.Code.CodeVar (CodeVarChunk, quantvar)

-- FIXME: These extractors are crucial to code generation!!! They don't look
-- like they should be needed at all!

-- | Get a list of 'CodeChunk's from an equation.
codevars :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars e m = map (varResolve m) $ eDep e

-- | Get a list of 'CodeChunk's from an equation (no functions).
codevars' :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars' e m = map (varResolve m) $ eDep' e

-- | Make a 'CodeVarChunk' from a 'UID' in the 'ChunkDB'.
varResolve :: ChunkDB -> UID -> CodeVarChunk
varResolve  m x = quantvar (findOrErr x m :: DefinedQuantityDict)
