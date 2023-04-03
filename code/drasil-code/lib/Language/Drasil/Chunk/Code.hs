{-# OPTIONS_GHC -Wno-orphans #-}

-- | Defines chunk types for use in code generation.
module Language.Drasil.Chunk.Code (
  CodeIdea(..), CodeChunk(..), CodeVarChunk(..), CodeFuncChunk(..), 
  VarOrFunc(..), obv, quantvar, quantfunc, ccObjVar, codevars, codevars', 
  funcResolve, varResolve, listToArray, programName, funcPrefix,
  DefiningCodeExpr(..)
) where

import Control.Lens ((^.), view)

import Language.Drasil
import Language.Drasil.Chunk.CodeBase
import Language.Drasil.Printers (symbolDoc)

import Text.PrettyPrint.HughesPJ (render)

-- | Finds the code name of a 'CodeChunk'.
instance CodeIdea    CodeChunk where
  codeName = render . symbolDoc . codeSymb . view qc
  codeChunk = id

-- | Finds the code name and 'CodeChunk' within a 'CodeVarChunk'.
instance CodeIdea    CodeVarChunk where 
  codeName = codeName . view ccv
  codeChunk c = CodeC (view qc $ view ccv c) Var

-- | Finds the code name and 'CodeChunk' within a 'CodeFuncChunk'.
instance CodeIdea    CodeFuncChunk where 
  codeName = codeName . view ccf
  codeChunk c = CodeC (view qc $ view ccf c) Func

-- | Combine an Object-type 'CodeChunk' with another 'CodeChunk' to create a new 
-- 'CodeChunk' which represents a field of the first. ex. @ccObjVar obj f = obj.f@.
ccObjVar :: CodeVarChunk -> CodeVarChunk -> CodeVarChunk
ccObjVar c1 c2 = checkObj (c1 ^. typ)
  where checkObj (Actor _) = CodeVC (codeChunk c2) (Just $ codeChunk c1)
        checkObj _ = error "First CodeChunk passed to ccObjVar must have Actor space"
