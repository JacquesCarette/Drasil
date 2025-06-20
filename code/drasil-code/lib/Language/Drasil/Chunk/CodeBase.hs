module Language.Drasil.Chunk.CodeBase where

import Database.Drasil (ChunkDB, symbResolve)
import Drasil.Code.CodeExpr.Development
import Language.Drasil

-- | Construct a 'CodeVarChunk' from a 'Quantity'.
quantvar :: (Quantity c, MayHaveUnit c) => c -> CodeVarChunk
quantvar c = CodeVC (CodeC (qw c) Var) Nothing

-- | Construct a 'CodeFuncChunk' from a 'Quantity'.
quantfunc :: (Quantity c, MayHaveUnit c) => c -> CodeFuncChunk
quantfunc c = CodeFC $ CodeC (qw c) Func

-- | Get a list of 'CodeChunk's from an equation.
codevars :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars e m = map (varResolve m) $ eDep e

-- | Get a list of 'CodeChunk's from an equation (no functions).
codevars' :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars' e m = map (varResolve m) $ eDep' e

-- | Make a 'CodeVarChunk' from a 'UID' in the 'ChunkDB'.
varResolve :: ChunkDB -> UID -> CodeVarChunk
varResolve  m x = quantvar $ symbResolve m x

-- | Make a 'CodeFuncChunk' from a 'UID' in the 'ChunkDB'.
funcResolve :: ChunkDB -> UID -> CodeFuncChunk
funcResolve m x = quantfunc $ symbResolve m x