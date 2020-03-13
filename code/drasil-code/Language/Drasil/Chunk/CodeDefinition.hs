{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.CodeDefinition (
  CodeDefinition, DefinitionType(..), qtoc, qtov, odeDef, auxExprs, defType, 
  codeEquat
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk(..), CodeIdea(codeName, codeChunk),
  VarOrFunc(..), quantvar, quantfunc, funcPrefix)
import Language.Drasil.Data.ODEInfo (ODEInfo(..), ODEOptions(..))

import Control.Lens ((^.), makeLenses, view)

data DefinitionType = Definition | ODE

data CodeDefinition = CD { _cchunk :: CodeChunk
                         , _def :: Expr
                         , _auxExprs :: [Expr]
                         , _defType :: DefinitionType
                         }
makeLenses ''CodeDefinition

instance HasUID       CodeDefinition where uid = cchunk . uid
instance NamedIdea    CodeDefinition where term = cchunk . term
instance Idea         CodeDefinition where getA = getA . view cchunk
instance HasSpace     CodeDefinition where typ = cchunk . typ
instance HasSymbol    CodeDefinition where symbol c = symbol (c ^. cchunk)
instance CodeIdea     CodeDefinition where 
  codeName (CD c@(CodeC _ Var) _ _ _) = codeName c
  codeName (CD c@(CodeC _ Func) _ _ _) = funcPrefix ++ codeName c
  codeChunk = view cchunk
instance Eq           CodeDefinition where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit  CodeDefinition where getUnit = getUnit . view cchunk
instance DefiningExpr CodeDefinition where defnExpr = def

qtoc :: (Quantity q, DefiningExpr q, MayHaveUnit q) => q -> CodeDefinition
qtoc q = CD (codeChunk $ quantfunc q) (q ^. defnExpr) [] Definition

qtov :: QDefinition -> CodeDefinition
qtov q = CD (codeChunk $ quantvar q) (q ^. defnExpr) [] Definition

odeDef :: ODEInfo -> CodeDefinition
odeDef info = CD (codeChunk $ depVar info) (Matrix [odeSyst info]) 
  (map ($ info) [tInit, tFinal, initVal, absTol . odeOpts, relTol . odeOpts, 
  stepSize . odeOpts]) ODE

codeEquat :: CodeDefinition -> Expr
codeEquat cd = cd ^. def