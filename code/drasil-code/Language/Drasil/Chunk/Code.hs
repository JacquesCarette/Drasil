{-# LANGUAGE TemplateHaskell #-}
-- | Defines chunk types for use in code generation.
module Language.Drasil.Chunk.Code (
  CodeIdea(..), CodeChunk(..), CodeVarChunk(..), CodeFuncChunk(..), 
  VarOrFunc(..), obv, quantvar, quantfunc, ccObjVar, codevars, codevars', 
  funcResolve, varResolve, listToArray, ConstraintMap, constraintMap, 
  physLookup, sfwrLookup, programName, funcPrefix
) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil
import Database.Drasil (ChunkDB, symbResolve)
import Language.Drasil.Development

import Language.Drasil.Printers (symbolDoc, toPlainName)

import Data.List (nub)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ (render)

-- not using lenses for now
-- | A 'CodeIdea' must include some code and its name. 
class CodeIdea c where
  -- | Name of the idea.
  codeName      :: c -> String
  -- | Code chunk associated with the idea.
  codeChunk     :: c -> CodeChunk

-- | Convert the program name to an abbrevaited 'String' without any special characters.
programName :: CommonIdea c => c -> String
programName = toPlainName . abrv

-- | Used when a function name needs to be distinguishable from a variable name.
funcPrefix :: String
funcPrefix = "func_"
 
-- | Details if a piece of code is meant to be a variable or a function.
data VarOrFunc = Var | Func

-- | Basic chunk representation in the code generation context.
-- Contains a QuantityDict and the kind of code (variable or function).
data CodeChunk = CodeC { _qc :: QuantityDict
                       , kind :: VarOrFunc
                       }
makeLenses ''CodeChunk

-- | Finds the 'UID' of the 'QuantityDict' used to make the 'CodeChunk'.
instance HasUID      CodeChunk where uid = qc . uid
-- | Finds the term ('NP') of the 'QuantityDict' used to make the 'CodeChunk'.
instance NamedIdea   CodeChunk where term = qc . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'CodeChunk'.
instance Idea        CodeChunk where getA = getA . view qc
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'CodeChunk'.
instance HasSpace    CodeChunk where typ = qc . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'QuantityDict' used to make the 'CodeChunk'.
instance HasSymbol   CodeChunk where symbol = symbol . view qc
-- | 'CodeChunk's have a 'Quantity'.
instance Quantity    CodeChunk
-- | Finds the code name of a 'CodeChunk'.
instance CodeIdea    CodeChunk where
  codeName = render . symbolDoc . codeSymb . view qc
  codeChunk = id
-- | Equal if 'UID's are equal.
instance Eq          CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'QuantityDict' used to make the 'CodeChunk'.
instance MayHaveUnit CodeChunk where getUnit = getUnit . view qc
instance ExprRelat   CodeChunk where relat c = sy c

-- | Chunk representing a variable. The @obv@ field represents the object containing 
-- this variable, if it is an object field.
data CodeVarChunk = CodeVC {_ccv :: CodeChunk,
                            _obv :: Maybe CodeChunk}
makeLenses ''CodeVarChunk

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance HasUID      CodeVarChunk where uid = ccv . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance NamedIdea   CodeVarChunk where term = ccv . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'CodeVarChunk'.
instance Idea        CodeVarChunk where getA = getA . view ccv
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance HasSpace    CodeVarChunk where typ = ccv . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance HasSymbol   CodeVarChunk where symbol = symbol . view ccv
-- | 'CodeVarChunk's have a 'Quantity'.
instance Quantity    CodeVarChunk
-- | Finds the code name and 'CodeChunk' within a 'CodeVarChunk'.
instance CodeIdea    CodeVarChunk where 
  codeName = codeName . view ccv
  codeChunk c = CodeC (view qc $ view ccv c) Var
-- | Equal if 'UID's are equal.
instance Eq          CodeVarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance MayHaveUnit CodeVarChunk where getUnit = getUnit . view ccv

-- | Chunk representing a function.
newtype CodeFuncChunk = CodeFC {_ccf :: CodeChunk}
makeLenses ''CodeFuncChunk

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance HasUID      CodeFuncChunk where uid = ccf . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance NamedIdea   CodeFuncChunk where term = ccf . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance Idea        CodeFuncChunk where getA = getA . view ccf
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance HasSpace    CodeFuncChunk where typ = ccf . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance HasSymbol   CodeFuncChunk where symbol = symbol . view ccf
-- | 'CodeFuncChunk's have a 'Quantity'.
instance Quantity    CodeFuncChunk
-- | Functions are Callable.
instance Callable    CodeFuncChunk
-- | Finds the code name and 'CodeChunk' within a 'CodeFuncChunk'.
instance CodeIdea    CodeFuncChunk where 
  codeName = codeName . view ccf
  codeChunk c = CodeC (view qc $ view ccf c) Func
-- | Equal if 'UID's are equal.
instance Eq          CodeFuncChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance MayHaveUnit CodeFuncChunk where getUnit = getUnit . view ccf

-- | Construct a 'CodeVarChunk' from a 'Quantity'.
quantvar :: (Quantity c, MayHaveUnit c) => c -> CodeVarChunk
quantvar c = CodeVC (CodeC (qw c) Var) Nothing

-- | Construct a 'CodeFuncChunk' from a 'Quantity'.
quantfunc :: (Quantity c, MayHaveUnit c) => c -> CodeFuncChunk
quantfunc c = CodeFC $ CodeC (qw c) Func

-- | Combine an Object-type 'CodeChunk' with another 'CodeChunk' to create a new 
-- 'CodeChunk' which represents a field of the first. ex. @ccObjVar obj f = obj.f@.
ccObjVar :: CodeVarChunk -> CodeVarChunk -> CodeVarChunk
ccObjVar c1 c2 = checkObj (c1 ^. typ)
  where checkObj (Actor _) = CodeVC (codeChunk c2) (Just $ codeChunk c1)
        checkObj _ = error "First CodeChunk passed to ccObjVar must have Actor space"

-- | Get a list of 'CodeChunk's from an equation.
codevars :: Expr -> ChunkDB -> [CodeVarChunk]
codevars e m = map (varResolve m) $ eDep e

-- | Get a list of 'CodeChunk's from an equation (no functions).
codevars' :: Expr -> ChunkDB -> [CodeVarChunk]
codevars' e m = map (varResolve m) $ nub $ eNames' e -- TODO: it seems weird, but I'll have to check if this + above should be DisplayExprs

-- | Make a 'CodeVarChunk' from a 'UID' in the 'ChunkDB'.
varResolve :: ChunkDB -> UID -> CodeVarChunk
varResolve  m x = quantvar $ symbResolve m x

-- | Make a 'CodeFuncChunk' from a 'UID' in the 'ChunkDB'.
funcResolve :: ChunkDB -> UID -> CodeFuncChunk
funcResolve m x = quantfunc $ symbResolve m x

-- Changes a 'CodeVarChunk'\'s space from 'Vect' to 'Array'.
listToArray :: CodeVarChunk -> CodeVarChunk
listToArray c = newSpc (c ^. typ) 
  where newSpc (Vect t) = CodeVC (CodeC (implVar' (c ^. uid ++ "_array") 
          (c ^. term) (getA c) (Array t) (symbol c Implementation) (getUnit c)) 
          Var) (c ^. obv)
        newSpc _ = c

-- | Constraints map. Contains all 'Constraint's.
type ConstraintMap = Map.Map UID [Constraint]

-- | Creates a map from 'UID' to 'Constraint's for constrained chunks.
constraintMap :: (HasUID c, Constrained c) => [c] -> ConstraintMap
constraintMap = Map.fromList . map (\x -> (x ^. uid, x ^. constraints))

-- | Returns a pair of a chunk and its physical constraints.
physLookup :: (HasUID q) => ConstraintMap -> q -> (q,[Constraint])
physLookup m q = constraintLookup' q m (filter isPhysC)

-- | Returns a pair of a chunk and its software constraints.
sfwrLookup :: (HasUID q) => ConstraintMap -> q -> (q,[Constraint])
sfwrLookup m q = constraintLookup' q m (filter isSfwrC)

-- | Returns a chunk and a filtered list of its constraints.
constraintLookup' :: (HasUID q) => q -> ConstraintMap
                      -> ([Constraint] -> [Constraint]) -> (q , [Constraint])
constraintLookup' q m filt = (q, maybe [] filt (Map.lookup (q^.uid) m))
