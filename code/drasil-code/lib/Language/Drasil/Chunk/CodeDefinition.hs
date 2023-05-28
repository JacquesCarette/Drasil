{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.CodeDefinition (
  CodeDefinition, DefinitionType(..), qtoc, qtov, odeDef, auxExprs, defType,
) where

import Language.Drasil 
import Language.Drasil.Chunk.Code (quantvar, quantfunc)
import Language.Drasil.CodeExpr.Development (expr, CanGenCode(..))
import Language.Drasil.Data.ODEInfo (ODEInfo(..), ODEOptions(..))

import Control.Lens ((^.), makeLenses, view)

-- | The definition may be specialized to use ODEs.
data DefinitionType = Definition | ODE

-- | A chunk for pairing a mathematical definition with a 'CodeChunk'.
data CodeDefinition = CD { _cchunk   :: CodeChunk
                         , _def      :: CodeExpr
                         , _auxExprs :: [CodeExpr]
                         , _defType  :: DefinitionType
                         }
makeLenses ''CodeDefinition

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'CodeDefinition'.
instance HasUID           CodeDefinition where uid = cchunk . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'CodeDefinition'.
instance NamedIdea        CodeDefinition where term = cchunk . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'CodeDefinition'.
instance Idea             CodeDefinition where getA = getA . view cchunk
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'CodeDefinition'.
instance HasSpace         CodeDefinition where typ = cchunk . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'CodeDefinition'.
instance HasSymbol        CodeDefinition where symbol c = symbol (c ^. cchunk)
-- | 'CodeDefinition's have a 'Quantity'.
instance Quantity         CodeDefinition
-- | Finds the code name of a 'CodeDefinition'.
-- 'Function' 'CodeDefinition's are named with the function prefix to distinguish 
-- them from the corresponding variable version.
instance CodeIdea         CodeDefinition where
  codeName (CD c@(CodeC _ Var) _ _ _) = codeName c
  codeName (CD c@(CodeC _ Func) _ _ _) = funcPrefix ++ codeName c
  codeChunk = view cchunk
-- | Equal if 'UID's are equal.
instance Eq               CodeDefinition where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'CodeDefinition'.
instance MayHaveUnit      CodeDefinition where getUnit = getUnit . view cchunk
-- | Finds the defining expression of a CodeDefinition.
instance DefiningCodeExpr CodeDefinition where codeExpr = def

-- NOTE: We'll eventually want extra smart constructors that allow for custom
--       CodeExprs inputs.

-- TODO: These below 3 functions that generate ``CodeDefinitions'' should be generalized
--       It _might_ be good to create make a ``CanGenCodeDefinition''-like typeclass

-- | Constructs a 'CodeDefinition' where the underlying 'CodeChunk' is for a function.
qtoc :: (Quantity (q Expr), MayHaveUnit (q Expr), DefiningExpr q) => q Expr -> CodeDefinition
qtoc q = CD (codeChunk $ quantfunc q) (expr $ q ^. defnExpr) [] Definition

-- | Constructs a 'CodeDefinition' where the underlying 'CodeChunk' is for a variable.
qtov :: CanGenCode e => QDefinition e -> CodeDefinition
qtov q = CD (codeChunk $ quantvar q) (toCodeExpr $ q ^. defnExpr) [] Definition

-- | Constructs a 'CodeDefinition' for an ODE.
odeDef :: ODEInfo -> CodeDefinition
odeDef info = CD 
  (codeChunk $ quantfunc $ depVar info)
  (matrix [odeSyst info])
  (matrix [initVal info]:
    map ($ info) [tInit, tFinal, absTol . odeOpts, relTol . odeOpts, stepSize . odeOpts])
  ODE
