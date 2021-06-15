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

-- | The definition may be specialized to use ODEs.
data DefinitionType = Definition | ODE

-- | A chunk for pairing a mathematical definition with a 'CodeChunk'.
data CodeDefinition = CD { _cchunk :: CodeChunk
                         , _def :: Expr
                         , _auxExprs :: [Expr]
                         , _defType :: DefinitionType
                         }
makeLenses ''CodeDefinition

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'CodeDefinition'.
instance HasUID       CodeDefinition where uid = cchunk . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'CodeDefinition'.
instance NamedIdea    CodeDefinition where term = cchunk . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'CodeDefinition'.
instance Idea         CodeDefinition where getA = getA . view cchunk
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'CodeDefinition'.
instance HasSpace     CodeDefinition where typ = cchunk . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'CodeDefinition'.
instance HasSymbol    CodeDefinition where symbol c = symbol (c ^. cchunk)
-- | 'CodeDefinition's have a 'Quantity'.
instance Quantity     CodeDefinition
-- | Finds the code name of a 'CodeDefinition'.
-- 'Function' 'CodeDefinition's are named with the function prefix to distinguish 
-- them from the corresponding variable version.
instance CodeIdea     CodeDefinition where 
  codeName (CD c@(CodeC _ Var) _ _ _) = codeName c
  codeName (CD c@(CodeC _ Func) _ _ _) = funcPrefix ++ codeName c
  codeChunk = view cchunk
-- | Equal if 'UID's are equal.
instance Eq           CodeDefinition where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'CodeDefinition'.
instance MayHaveUnit  CodeDefinition where getUnit = getUnit . view cchunk
instance Display      CodeDefinition where toDispExpr c = defines (c ^. cchunk) (c ^. def) 
-- | Finds the defining expression of a CodeDefinition.
instance DefiningExpr CodeDefinition where defnExpr = def

-- | Constructs a 'CodeDefinition' where the underlying 'CodeChunk' is for a function.
qtoc :: (Quantity q, DefiningExpr q, MayHaveUnit q) => q -> CodeDefinition
qtoc q = CD (codeChunk $ quantfunc q) (q ^. defnExpr) [] Definition

-- | Constructs a 'CodeDefinition' where the underlying 'CodeChunk' is for a variable.
qtov :: QDefinition -> CodeDefinition
qtov q = CD (codeChunk $ quantvar q) (q ^. defnExpr) [] Definition

-- | Constructs a 'CodeDefinition' for an ODE.
odeDef :: ODEInfo -> CodeDefinition
odeDef info = CD (codeChunk $ quantfunc $ depVar info) (Matrix [odeSyst info]) 
  (map ($ info) [tInit, tFinal, initVal, absTol . odeOpts, relTol . odeOpts, 
  stepSize . odeOpts, initValFstOrd . odeOpts]) ODE

-- | Returns the defining Expr of a 'CodeDefinition'.
codeEquat :: CodeDefinition -> Expr
codeEquat cd = cd ^. def