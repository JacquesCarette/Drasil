{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
    -- * Chunk Type
    DifferentialModel,
    -- * Input Language
    ($^^),
    -- * Constructors
    makeASystemDE
    ) where

import Control.Lens (makeLenses, (^.), view)
import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS)
import Language.Drasil.UID (HasUID(uid))
import Language.Drasil.Classes (Express(..),
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Chunk.Unital (UnitalChunk)
import Language.Drasil.ModelExpr.Class (ModelExprC(nthderiv, equiv))
import Language.Drasil.Expr.Class (mulRe, addRe, sy, ExprC (($.), matrix, columnVec))
import Language.Drasil.Chunk.Constrained (ConstrConcept)
import Language.Drasil.Chunk.Quantity (qw)

-- Unknown is a index of dependent variables with its nth degree of derivative
data Unknown = UK{
  _depVar :: ConstrConcept,
  _degree :: Int
}
makeLenses ''Unknown

{-
  Input Language represent a derivative of a dependent variable
  e.g. depVar $^^ d, 
  depVar is the dependent variable, d is the dth derivative
-}
($^^) :: ConstrConcept -> Int -> Unknown
($^^) = UK

data DifferentialModel = SystemOfLinearODEs {
  -- independent variable, usually time
  _indepVar :: UnitalChunk,
  -- coefficients matrix
  _coefficients :: [[Expr]],
  -- Unknowns column vector
  _unknowns :: [Unknown],
  -- Constant Column vector 
  _constants :: [Expr],
  _conc :: ConceptChunk
}
makeLenses ''DifferentialModel

-- | Finds the 'UID' of the 'ConceptChunk' used to make the 'DifferentialModel'.
instance HasUID        DifferentialModel where uid = conc . uid
-- | Equal if 'UID's are equal.
instance Eq            DifferentialModel where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the term ('NP') of the 'ConceptChunk' used to make the 'DifferentialModel'.
instance NamedIdea     DifferentialModel where term = conc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'DifferentialModel'.
instance Idea          DifferentialModel where getA = getA . view conc
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'DifferentialModel'.
instance Definition    DifferentialModel where defn = conc . defn
-- | Finds the domain of the 'ConceptChunk' used to make the 'DifferentialModel'.
instance ConceptDomain DifferentialModel where cdom = cdom . view conc
-- | Convert the 'DifferentialModel' into the model expression language.
instance Express       DifferentialModel where express = formStdODE

-- | Set the expression be a system of linear ODE to Ax = b
-- | A is a known m*n matrix that contains coefficients, 
-- | x is an n-vector that contain derivatives of dependent variables
-- | b is an m-vector that contain constants
formStdODE :: DifferentialModel -> ModelExpr
formStdODE d 
    | size == 1 = formASingleODE (head (d ^. coefficients)) unknownVec (d ^. constants)
    | otherwise = equiv (coeffsMatix $. columnVec unknownVec : constantVec) 
    where size = length (d ^. coefficients)
          coeffsMatix = express(matrix (d ^. coefficients))
          unknownVec = formAllUnknown (d ^. unknowns) (d ^. indepVar)
          constantVec = [express (columnVec (d ^. constants))]

formASingleODE :: [Expr] -> [ModelExpr] -> [Expr] -> ModelExpr
formASingleODE coeffs unks consts = equiv (lhs : rhs)
  where lhs = foldl1 addRe (zipWith (\x y -> (express x) `mulRe` y) coeffs unks)
        rhs = map express consts

-- Form a n-vector of derivatives dependent variables
formAllUnknown :: [Unknown] -> UnitalChunk -> [ModelExpr]
formAllUnknown unks ind = map (`formAUnknown` ind) unks

-- Form a derivative of a dependent variable
formAUnknown :: Unknown -> UnitalChunk -> ModelExpr
formAUnknown unk = nthderiv
                    (toInteger (unk ^. degree))
                    (sy (qw (unk ^. depVar)))

-- | Create a 'DifferentialModel' from a given indepVar ('UnitalChunk'), coefficients ('[[Expr]]'),
-- | unknowns ('[Unknown]'), constants ('[Expr]'), UID ('String'), term ('NP'), definition ('Sentence').
makeASystemDE :: UnitalChunk -> [[Expr]] -> [Unknown] -> [Expr]-> String -> NP -> Sentence -> DifferentialModel
makeASystemDE dmIndepVar dmcoeffs dmUnk dmConst dmID dmTerm dmDefn 
 | length dmcoeffs /= length dmConst = 
  error "Length of coefficients matrix should equal to the length of the constant vector"
 | not $ isCoeffsMatchUnknowns dmcoeffs dmUnk = 
  error "The length of each row vector in coefficients need to equal to the length of unknowns vector"
 | otherwise = SystemOfLinearODEs dmIndepVar dmcoeffs dmUnk dmConst(dccWDS dmID dmTerm dmDefn)

isCoeffsMatchUnknowns :: [[Expr]] -> [Unknown] -> Bool
isCoeffsMatchUnknowns [] _ = error "Coefficients matrix can not be empty"
isCoeffsMatchUnknowns _ [] = error "Unknowns column vector can not be empty"
isCoeffsMatchUnknowns xs unks = foldr (\ x -> (&&) (length x == length unks)) True xs

