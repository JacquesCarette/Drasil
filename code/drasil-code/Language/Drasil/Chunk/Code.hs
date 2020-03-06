{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeChunk(..), CodeVarChunk(..), CodeFuncChunk(..), 
    VarOrFunc(..), codevarC, codefuncC, codevar, codefunc, quantvar, quantfunc,
    ccObjVar, codevars, codevars', funcResolve, varResolve, ConstraintMap, 
    constraintMap, physLookup, sfwrLookup, programName, funcPrefix
  ) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil
import Database.Drasil (ChunkDB, symbResolve)
import Language.Drasil.Development (dep, names')

import Language.Drasil.Chunk.CodeQuantity (CodeQuantityDict, cqw, implCQD)
import Language.Drasil.Printers (symbolDoc, toPlainName)

import Data.List (nub)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ (render)

-- not using lenses for now
class CodeIdea c where
  codeName      :: c -> String
  codeChunk     :: c -> CodeChunk

programName :: CommonIdea c => c -> String
programName = toPlainName . abrv

funcPrefix :: String
funcPrefix = "func_"
 
data VarOrFunc = Var | Func
data CodeChunk = CodeC { _qc :: CodeQuantityDict
                       , kind :: VarOrFunc
                       }
makeLenses ''CodeChunk

instance HasUID      CodeChunk where uid = qc . uid
instance NamedIdea   CodeChunk where term = qc . term
instance Idea        CodeChunk where getA = getA . view qc
instance HasSpace    CodeChunk where typ = qc . typ
instance HasSymbol   CodeChunk where symbol = symbol . view qc
instance CodeIdea    CodeChunk where
  codeName (CodeC c Var) = render $ symbolDoc (codeSymb c)
  codeName (CodeC c Func) = funcPrefix ++ render (symbolDoc (codeSymb c))
  codeChunk = id
instance Eq          CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeChunk where getUnit = getUnit . view qc

newtype CodeVarChunk = CodeVC {_ccv :: CodeChunk}
makeLenses ''CodeVarChunk

instance HasUID      CodeVarChunk where uid = ccv . uid
instance NamedIdea   CodeVarChunk where term = ccv . term
instance Idea        CodeVarChunk where getA = getA . view ccv
instance HasSpace    CodeVarChunk where typ = ccv . typ
instance HasSymbol   CodeVarChunk where symbol = symbol . view ccv
instance CodeIdea    CodeVarChunk where 
  codeName = codeName . view ccv
  codeChunk = codeChunk . view ccv
instance Eq          CodeVarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeVarChunk where getUnit = getUnit . view ccv

newtype CodeFuncChunk = CodeFC {_ccf :: CodeChunk}
makeLenses ''CodeFuncChunk

instance HasUID      CodeFuncChunk where uid = ccf . uid
instance NamedIdea   CodeFuncChunk where term = ccf . term
instance Idea        CodeFuncChunk where getA = getA . view ccf
instance HasSpace    CodeFuncChunk where typ = ccf . typ
instance HasSymbol   CodeFuncChunk where symbol = symbol . view ccf
instance Callable    CodeFuncChunk
instance CodeIdea    CodeFuncChunk where 
  codeName = codeName . view ccf
  codeChunk = codeChunk . view ccf
instance Eq          CodeFuncChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeFuncChunk where getUnit = getUnit . view ccf

codevarC :: (CodeIdea c) => c -> CodeVarChunk
codevarC = CodeVC . codeChunk

codefuncC :: (CodeIdea c) => c -> CodeFuncChunk
codefuncC = CodeFC . codeChunk

codevar :: CodeQuantityDict -> CodeVarChunk
codevar c = CodeVC $ CodeC c Var

codefunc :: CodeQuantityDict -> CodeFuncChunk
codefunc c = CodeFC $ CodeC c Func

quantvar :: (Quantity c, MayHaveUnit c) => c -> CodeVarChunk
quantvar c = CodeVC $ CodeC (cqw c) Var

quantfunc :: (Quantity c, MayHaveUnit c) => c -> CodeFuncChunk
quantfunc c = CodeFC $ CodeC (cqw c) Func

-- Combine an Object-type CodeChunk with another CodeChunk to create a new 
-- CodeChunk which represents a field of the first. ex. ccObjVar obj f = obj.f
ccObjVar :: CodeVarChunk -> CodeVarChunk -> CodeVarChunk
ccObjVar c1 c2 = checkObj (c1 ^. typ)
  where checkObj (Actor _) = codevar $ implCQD (c1 ^. uid ++ "." ++ c2 ^. uid) 
          (compoundPhrase (c1 ^. term) (c2 ^. term)) Nothing (c2 ^. typ) 
          (Concat [symbol c1 Implementation, Label ".", symbol c2 
          Implementation]) (getUnit c2) 
        checkObj _ = error "First CodeChunk passed to ccObjVar must have Actor space"

-- | Get a list of CodeChunks from an equation
codevars :: Expr -> ChunkDB -> [CodeVarChunk]
codevars e m = map (varResolve m) $ dep e

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: Expr -> ChunkDB -> [CodeVarChunk]
codevars' e m = map (varResolve m) $ nub $ names' e

varResolve :: ChunkDB -> UID -> CodeVarChunk
varResolve  m x = quantvar $ symbResolve m x

funcResolve :: ChunkDB -> UID -> CodeFuncChunk
funcResolve m x = quantfunc $ symbResolve m x

type ConstraintMap = Map.Map UID [Constraint]

constraintMap :: (HasUID c, Constrained c) => [c] -> ConstraintMap
constraintMap = Map.fromList . map (\x -> (x ^. uid, x ^. constraints))

physLookup :: (HasUID q) => ConstraintMap -> q -> (q,[Constraint])
physLookup m q = constraintLookup' q m (filter isPhysC)

sfwrLookup :: (HasUID q) => ConstraintMap -> q -> (q,[Constraint])
sfwrLookup m q = constraintLookup' q m (filter isSfwrC)

constraintLookup' :: (HasUID q) => q -> ConstraintMap
                      -> ([Constraint] -> [Constraint]) -> (q , [Constraint])
constraintLookup' q m filt = (q, maybe [] filt (Map.lookup (q^.uid) m))
