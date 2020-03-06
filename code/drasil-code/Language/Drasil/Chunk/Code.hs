{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeChunk(..), VarOrFunc(..), codevar, codefunc, quantvar, 
    quantfunc, ccObjVar, codevars, codevars', funcResolve, varResolve, 
    ConstraintMap, constraintMap, physLookup, sfwrLookup, programName, 
    funcPrefix
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
instance HasSymbol   CodeChunk where symbol c = symbol (c ^. qc)
instance CodeIdea    CodeChunk where
  codeName (CodeC c Var) = render $ symbolDoc (codeSymb c)
  codeName (CodeC c Func) = funcPrefix ++ render (symbolDoc (codeSymb c))
  codeChunk = id
instance Eq          CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeChunk where getUnit = getUnit . view qc

codevar :: CodeQuantityDict -> CodeChunk
codevar c = CodeC c Var

codefunc :: CodeQuantityDict -> CodeChunk
codefunc c = CodeC c Func

quantvar :: (Quantity c, MayHaveUnit c) => c -> CodeChunk
quantvar c = CodeC (cqw c) Var

quantfunc :: (Quantity c, MayHaveUnit c) => c -> CodeChunk
quantfunc c = CodeC (cqw c) Func

-- Combine an Object-type CodeChunk with another CodeChunk to create a new 
-- CodeChunk which represents a field of the first. ex. ccObjVar obj f = obj.f
ccObjVar :: CodeChunk -> CodeChunk -> CodeChunk
ccObjVar c1 c2 = checkObj (c1 ^. typ)
  where checkObj (Actor _) = codevar $ implCQD (c1 ^. uid ++ "." ++ c2 ^. uid) 
          (compoundPhrase (c1 ^. term) (c2 ^. term)) Nothing (c2 ^. typ) 
          (Concat [symbol c1 Implementation, Label ".", symbol c2 
          Implementation]) (getUnit c2) 
        checkObj _ = error "First CodeChunk passed to ccObjVar must have Actor space"

-- | Get a list of CodeChunks from an equation
codevars :: Expr -> ChunkDB -> [CodeChunk]
codevars e m = map (varResolve m) $ dep e

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: Expr -> ChunkDB -> [CodeChunk]
codevars' e m = map (varResolve m) $ nub $ names' e

funcResolve, varResolve :: ChunkDB -> UID -> CodeChunk
funcResolve m x = quantfunc $ symbResolve m x
varResolve  m x = quantvar  $ symbResolve m x

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
