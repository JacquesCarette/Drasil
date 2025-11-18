module Language.Drasil.Chunk.CodeBase where

import Drasil.Database (ChunkDB, findOrErr)
import Drasil.Code.CodeExpr.Development
import Language.Drasil

-- FIXME: Everything in this file needs to be deleted.
--
-- Notes:
--
-- 1. `quantvar` is massively overused in `drasil-code` and only embeds a DQD
-- inside a `CodeVarChunk`. It adds little information.
--
-- 2.`quantfunc` is similar to `quantvar`, just not used as much and builds a
-- `CodeFuncChunk`.
--
-- 3. The remaining functions are for searching data types for what chunk
-- references they contain.
--
-- All of these things are common re-occurring patterns and should be done in
-- better ways. How? Not sure yet!

-- | Construct a 'CodeVarChunk' from a 'Quantity'.
quantvar :: (Quantity c, MayHaveUnit c, Concept c) => c -> CodeVarChunk
quantvar c = CodeVC (CodeC (dqdWr c) Var) Nothing

-- | Construct a 'CodeFuncChunk' from a 'Quantity'.
quantfunc :: (Quantity c, MayHaveUnit c, Concept c) => c -> CodeFuncChunk
quantfunc c = CodeFC $ CodeC (dqdWr c) Func

-- | Get a list of 'CodeChunk's from an equation.
codevars :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars e m = map (varResolve m) $ eDep e

-- | Get a list of 'CodeChunk's from an equation (no functions).
codevars' :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars' e m = map (varResolve m) $ eDep' e

-- | Make a 'CodeVarChunk' from a 'UID' in the 'ChunkDB'.
varResolve :: ChunkDB -> UID -> CodeVarChunk
varResolve  m x = quantvar (findOrErr x m :: DefinedQuantityDict)

-- | Make a 'CodeFuncChunk' from a 'UID' in the 'ChunkDB'.
funcResolve :: ChunkDB -> UID -> CodeFuncChunk
funcResolve m x = quantfunc (findOrErr x m :: DefinedQuantityDict)
