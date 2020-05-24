{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
  CodeIdea(..), CodeChunk(..), CodeVarChunk(..), CodeFuncChunk(..), 
  VarOrFunc(..), obv, quantvar, quantfunc, ccObjVar, codevars, codevars', 
  funcResolve, varResolve, listToArray, ConstraintMap, constraintMap, 
  physLookup, sfwrLookup, programName, funcPrefix
) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil
import Database.Drasil (ChunkDB, symbResolve)
import Language.Drasil.Development (dep, names')

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
data CodeChunk = CodeC { _qc :: QuantityDict
                       , kind :: VarOrFunc
                       }
makeLenses ''CodeChunk

instance HasUID      CodeChunk where uid = qc . uid
instance NamedIdea   CodeChunk where term = qc . term
instance Idea        CodeChunk where getA = getA . view qc
instance HasSpace    CodeChunk where typ = qc . typ
instance HasSymbol   CodeChunk where symbol = symbol . view qc
instance Quantity    CodeChunk
instance CodeIdea    CodeChunk where
  codeName = render . symbolDoc . codeSymb . view qc
  codeChunk = id
instance Eq          CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeChunk where getUnit = getUnit . view qc

data CodeVarChunk = CodeVC {_ccv :: CodeChunk,
                            _obv :: Maybe CodeChunk}
makeLenses ''CodeVarChunk

instance HasUID      CodeVarChunk where uid = ccv . uid
instance NamedIdea   CodeVarChunk where term = ccv . term
instance Idea        CodeVarChunk where getA = getA . view ccv
instance HasSpace    CodeVarChunk where typ = ccv . typ
instance HasSymbol   CodeVarChunk where symbol = symbol . view ccv
instance Quantity    CodeVarChunk
instance CodeIdea    CodeVarChunk where 
  codeName = codeName . view ccv
  codeChunk c = CodeC (view qc $ view ccv c) Var
instance Eq          CodeVarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeVarChunk where getUnit = getUnit . view ccv

newtype CodeFuncChunk = CodeFC {_ccf :: CodeChunk}
makeLenses ''CodeFuncChunk

instance HasUID      CodeFuncChunk where uid = ccf . uid
instance NamedIdea   CodeFuncChunk where term = ccf . term
instance Idea        CodeFuncChunk where getA = getA . view ccf
instance HasSpace    CodeFuncChunk where typ = ccf . typ
instance HasSymbol   CodeFuncChunk where symbol = symbol . view ccf
instance Quantity    CodeFuncChunk
instance Callable    CodeFuncChunk
instance CodeIdea    CodeFuncChunk where 
  codeName = codeName . view ccf
  codeChunk c = CodeC (view qc $ view ccf c) Func
instance Eq          CodeFuncChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeFuncChunk where getUnit = getUnit . view ccf

quantvar :: (Quantity c, MayHaveUnit c) => c -> CodeVarChunk
quantvar c = CodeVC (CodeC (qw c) Var) Nothing

quantfunc :: (Quantity c, MayHaveUnit c) => c -> CodeFuncChunk
quantfunc c = CodeFC $ CodeC (qw c) Func

-- Combine an Object-type CodeChunk with another CodeChunk to create a new 
-- CodeChunk which represents a field of the first. ex. ccObjVar obj f = obj.f
ccObjVar :: CodeVarChunk -> CodeVarChunk -> CodeVarChunk
ccObjVar c1 c2 = checkObj (c1 ^. typ)
  where checkObj (Actor _) = CodeVC (codeChunk c2) (Just $ codeChunk c1)
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

listToArray :: CodeVarChunk -> CodeVarChunk
listToArray c = newSpc (c ^. typ) 
  where newSpc (Vect t) = CodeVC (CodeC (implVar' (c ^. uid ++ "_array") 
          (c ^. term) (getA c) (Array t) (symbol c Implementation) (getUnit c)) 
          Var) (c ^. obv)
        newSpc _ = c

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
