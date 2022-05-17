{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
    -- * Chunk Type
    DifferentialModel,
    -- * Input Language
    ($^^), ($*), ($+),
    -- * Constructors
    makeASystemDE, makeASingleDE
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
import Language.Drasil.Literal.Class (LiteralC(exactDbl))
import Data.List (find)

-- Unknown is nth degree of derivative of its dependent variables 
data Unknown = UK{
  _depVar :: ConstrConcept,
  _degree :: Int
}
makeLenses ''Unknown

-- Term is the relation of a coefficient and an unknown
data Term = T{
  _coeff :: Expr,
  _unk :: Unknown
}
makeLenses ''Term

-- LHS is a collection of Terms
type LHS = [Term]

{-
  Input Language represent a derivative of a dependent variable
  e.g. depVar $^^ d, 
  depVar is the dependent variable, d is the dth derivative
-}
($^^) :: ConstrConcept -> Int -> Unknown
($^^) = UK

{-
  Input Language represent Term which a coefficient multiple an unknown
  e.g. exactDbl 1 $* (opProcessVariable $^^ 2), 
  exactDbl 1 is the the coefficient, 
  (opProcessVariable $^^ 2) is the 2rd derivative of opProcessVariable
-}
($*) :: Expr -> Unknown -> Term
($*) = T

{-
  Input Language represent a collection of Terms
  e.g. [exactDbl 1 $* (opProcessVariable $^^ 2)]
       $+ (exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1))
  [exactDbl 1 $* (opProcessVariable $^^ 2)] is a collection with a single Term, 
  (exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1)) is the appended element
-}
($+) :: [Term] -> Term -> LHS
($+) xs x  = xs ++ [x]

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

data InitialValueProblem = IVP{
}

data ODESolverFormat = ODESolverFormat{
  coefficients1 :: [[Expr]],
  unknowns1 :: [Int],
  constants1 :: [Expr]
}

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
  where lhs = foldl1 addRe (map (\x-> express (fst x) `mulRe` snd x) $ filterZeroCoeff coeffs unks)
        rhs = map express consts

-- Remove zero coefficients for display purpose
filterZeroCoeff :: [Expr] -> [ModelExpr] -> [(Expr, ModelExpr)]
filterZeroCoeff es mes = filter (\x -> fst x /= exactDbl 0) $ zip es mes

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

-- | Create a single ODE with its left hand side and right hand side
makeASingleDE :: UnitalChunk -> LHS -> Expr-> String -> NP -> Sentence -> DifferentialModel
makeASingleDE dmIndepVar dmLHS dmConst dmID dmTerm dmDefn
 | length dmcoeffs /= length [dmConst] =
  error "Length of coefficients matrix should equal to the length of the constant vector"
 | not $ isCoeffsMatchUnknowns dmcoeffs dmUnk =
  error "The length of each row vector in coefficients need to equal to the length of unknowns vector"
 | otherwise = SystemOfLinearODEs dmIndepVar dmcoeffs dmUnk [dmConst](dccWDS dmID dmTerm dmDefn)
  where dmUnk = createAllUnknowns(findHighestOrder dmLHS ^. unk)
        dmcoeffs = [createCoefficients dmLHS dmUnk]

-- | Function to check whether dimension of coefficient is match with the unknown vector
isCoeffsMatchUnknowns :: [[Expr]] -> [Unknown] -> Bool
isCoeffsMatchUnknowns [] _ = error "Coefficients matrix can not be empty"
isCoeffsMatchUnknowns _ [] = error "Unknowns column vector can not be empty"
isCoeffsMatchUnknowns coeffs unks = foldr (\ x -> (&&) (length x == length unks)) True coeffs

-- | Transform LHS to a collection of Expr
transToExprList :: LHS -> [Expr]
transToExprList = map (^. coeff)

-- | Transform LHS to a collection of Unknown
transToUnkList:: LHS -> [Unknown]
transToUnkList = map (^. unk)

-- | Find the highest order in left hand side
findHighestOrder :: LHS -> Term
findHighestOrder = foldr1 (\x y -> if x ^. (unk . degree) >= y ^. (unk . degree) then x else y)

-- | Create all possible unknowns based on the highest order
createAllUnknowns :: Unknown -> [Unknown]
createAllUnknowns highestUnk
  | highestUnk ^. degree  == 0  = [highestUnk]
  | otherwise = highestUnk : createAllUnknowns ((highestUnk ^. depVar) $^^ (highestUnk ^. degree - 1))

-- | Create Coefficients base on all possible unknowns
createCoefficients :: LHS -> [Unknown] -> [Expr]
createCoefficients [] _ = error "Left hand side is an empty list"
createCoefficients _ [] = []
createCoefficients lhs (x:xs) = genCoefficient (findCoefficient x lhs) : createCoefficients lhs xs

genCoefficient :: Maybe Term -> Expr
genCoefficient Nothing = exactDbl 0
genCoefficient (Just x) = x ^. coeff

findCoefficient :: Unknown -> LHS -> Maybe Term
findCoefficient u = find(\x -> x ^. (unk . depVar) == u ^. depVar && x ^. (unk . degree) == u ^. degree)

-- find the highest order in single ode
-- create all unknowns
-- check any unknowns not exist, add to zero coefficient

-- transDEToSolverFormat :: [[Expr]] -> [Unknown] -> [Expr] -> ODESolverFormat
-- transDEToSolverFormat map first and foldl with concatTwoSystemDE

--reduceHighOrderODE :: [Expr] -> [Unknown] -> [Expr] -> ODESolverFormat
--reduceHighOrderODE es us cs = 

-- get the hightest order

-- concatTwoSystemDE :: ODESolverFormat -> ODESolverFormat -> ODEReducedFormat
