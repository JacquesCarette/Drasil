{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeChunk(..), codeType, codevar, codefunc, quantvar, 
    quantfunc, ConstraintMap, constraintMap, physLookup, sfwrLookup,
    programName, funcPrefix
  ) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil

import Language.Drasil.Code.Code (CodeType)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType(ctyp), CodeQuantityDict, 
  cqw)
import Language.Drasil.Printers (symbolDoc, toPlainName)

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
instance HasCodeType CodeChunk where ctyp = qc . ctyp
instance HasSymbol   CodeChunk where symbol c = symbol (c ^. qc)
instance CodeIdea    CodeChunk where
  codeName (CodeC c Var) = render $ symbolDoc (codeSymb c)
  codeName (CodeC c Func) = funcPrefix ++ render (symbolDoc (codeSymb c))
  codeChunk = id
instance Eq          CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeChunk where getUnit = getUnit . view qc

codeType :: HasCodeType c => c -> CodeType
codeType c = c ^. ctyp

codevar :: CodeQuantityDict -> CodeChunk
codevar c = CodeC c Var

codefunc :: CodeQuantityDict -> CodeChunk
codefunc c = CodeC c Func

quantvar :: (Quantity c, MayHaveUnit c) => c -> CodeChunk
quantvar c = CodeC (cqw c) Var

quantfunc :: (Quantity c, MayHaveUnit c) => c -> CodeChunk
quantfunc c = CodeC (cqw c) Func

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
