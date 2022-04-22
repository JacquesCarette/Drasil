{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
    -- * Chunk Type
    DifferentialModel,
    -- * Input Language
    ($*),($^^),($+),
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
import Language.Drasil.Expr.Class (mulRe, addRe, sy, ExprC (($&&)))
import Language.Drasil.Chunk.Constrained (ConstrConcept)
import Language.Drasil.Chunk.Quantity (qw)
import Language.Drasil.Literal.Class (exactDbl)

-- Variable is a index of dependent variables with its nth degree of derivative
data Variable = V{
  _depVar :: ConstrConcept,
  _degree :: Int
}
makeLenses ''Variable

-- Term is a coefficient with list of its variables
data Term = T{
  _coeff :: Expr,
  _variables :: [Variable]
}
makeLenses ''Term

-- Polynomial is a list of term with its constant
data Polynomial = P{
  _terms :: [Term],
  _constant :: Expr
}
makeLenses ''Polynomial

{-
  Input Language mimic Variable
  e.g. depVar $^^ d, 
  depVar is the dependent variable, d is the dth derivative
-}
($^^) :: ConstrConcept -> Int -> Variable
($^^) = V

{-
  Input Language mimic Term
  e.g. c $* v, 
  c is the coefficient, v are all variables
-}
($*):: Expr -> [Variable] -> Term
($*) = T

{-
  Input Language mimic Polynomial
  e.g. t $+ c, 
  t are all terms, c is the constant
-}
($+) :: [Term] -> Expr -> Polynomial
($+) = P

data DifferentialModel = System {
  _indepVar :: UnitalChunk,
  _polynomials :: [Polynomial],
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
instance ConceptDomain DifferentialModel where cdom = cdom . view conc
-- | Finds the domain of the 'ConceptChunk' used to make the 'DifferentialModel'.
-- | Convert the 'DifferentialModel' into the model expression language.
-- | Set Canonical form of ODE to Zero, e.g. ax0 + bx1 + cx2 + .... + c = 0
instance Express       DifferentialModel where express = formStdODE

-- | Construct a Canonical form of System ODE with one or more polynomial DE, 
-- | A polynomial is sum of one or more variable multiplied by coefficients
-- | DE follow this format ax0 + bx1 + cx2 + .... + c = 0
-- | x0 is the highest order, x1 is the second highest order, and so on. The c is the constant.
formStdODE :: DifferentialModel -> ModelExpr
formStdODE d = formASingleExpr $ setAllPolyToZero $ formMultiPoly d

formMultiPoly :: DifferentialModel -> [ModelExpr]
formMultiPoly d = map (\x -> formAPolynomial x (d ^. indepVar)) (d ^. polynomials)

formASingleExpr :: [ModelExpr] -> ModelExpr
formASingleExpr = foldl1 ($&&)

setAllPolyToZero :: [ModelExpr] -> [ModelExpr]
setAllPolyToZero = map (\x -> equiv $ x : [exactDbl 0])

-- Form a variable in ModelExpr shape
formAVariable :: Variable -> UnitalChunk -> ModelExpr
formAVariable var = nthderiv
                    (toInteger (var ^. degree))
                    (sy (qw (var ^. depVar)))

-- Form a term in ModelExpr shape
formATerm :: Term -> UnitalChunk -> ModelExpr
formATerm t indepVar =
  express (t ^. coeff) `mulRe` multiAllVars (t ^.variables) indepVar

-- Form a polynomial in ModelExpr shape
formAPolynomial :: Polynomial-> UnitalChunk -> ModelExpr
formAPolynomial p indepVar =
  multiAllTerms (p ^. terms) indepVar `addRe` express (p ^. constant)

-- | Multiple all Terms
multiAllTerms :: [Term] -> UnitalChunk -> ModelExpr
multiAllTerms terms indepVar =
  foldl1 addRe
  $ map(`formATerm` indepVar) terms

-- | Multiple all Variables
multiAllVars :: [Variable] -> UnitalChunk -> ModelExpr
multiAllVars vars indepVar =
  foldl1 mulRe
  $ map (`formAVariable` indepVar) vars

-- | Construct a Canonical form of ODE, e.g. ax0 + bx1 + cx2 + .... + c
-- | x0 is the highest order, x1 is the second higest order, and so on. The c is the constant.
-- formStdODE :: DifferentialModel -> ModelExpr
-- formStdODE d = equiv $ (addCoes d `addRe` express (d ^. constant)) : [exactDbl 0]

-- | Construct a form of ODE with constant on the rhs
-- formConODE :: DifferentialModel -> ModelExpr
-- formConODE d = equiv $ addCoes d : [express (d ^. constant)]

-- | Add coefficients together by restructuring each CoeffDeriv
-- addCoes :: DifferentialModel -> ModelExpr
-- addCoes p = foldr1 addRe $
--             map(\x ->
--                   express (x ^. coeff)
--                   `mulRe`
--                   nthderiv
--                     (toInteger (x ^. degree))
--                     (sy (qw (d ^. depVar)))
--                     (d ^. indepVar)
--                )
--                (d ^. coefficients)

-- | Create a 'DifferentialModel' from a given indepVar ('UnitalChunk'), DepVar ('ModelExpr'),
-- | Coefficients ('[Expr]'), Constant ('Expr'), UID ('String'), term ('NP'), definition ('Sentence').
makeASystemDE :: UnitalChunk -> [Polynomial] -> String -> NP -> Sentence -> DifferentialModel
makeASystemDE dmIndepVar dmPoly dmID dmTerm dmDefn =
  System dmIndepVar dmPoly (dccWDS dmID dmTerm dmDefn)

-- getDepVar :: DifferentialModel -> [ConstrConcept]
-- getDepVar 

-- getParameters:: DifferentialModel -> [Quantity]
-- getParameters 