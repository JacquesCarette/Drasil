{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
    -- * Chunk Type
    DifferentialModel,
    -- * Input Language
    ($*),
    -- * Constructors
    makeLinear
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
import Language.Drasil.Expr.Class (mulRe, addRe, sy)
import Language.Drasil.Chunk.Constrained (ConstrConcept)
import Language.Drasil.Chunk.Quantity (qw)
import Language.Drasil.Literal.Class (exactDbl)

{-
  Input Language minic mathematic equation
  e.g. exactDbl 1 $* 1, 
  exactDbl 1 is coefficient term, 1 is the first derivative
-}

data CoeffDeriv = CD{
                      _coeff :: Expr,
                      _degree :: Int
                    }
makeLenses ''CoeffDeriv

($*) :: Expr -> Int -> CoeffDeriv
($*) = CD

data DifferentialModel = Linear {
                                  _indepVar :: UnitalChunk,
                                  _depVar :: ConstrConcept,
                                  _coefficients :: [CoeffDeriv],
                                  _constant :: Expr,
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

-- | Construct a Canonical form of ODE, e.g. ax0 + bx1 + cx2 + .... + c
-- | x0 is the highest order, x1 is the second higest order, and so on. The c is the constant.
formStdODE :: DifferentialModel -> ModelExpr
formStdODE d = equiv $ (addCoes d `addRe` express (d ^. constant)) : [exactDbl 0]

-- | Construct a form of ODE with constant on the rhs
-- formConODE :: DifferentialModel -> ModelExpr
-- formConODE d = equiv $ addCoes d : [express (d ^. constant)]

-- | Add coefficients together by restructuring each CoeffDeriv
addCoes :: DifferentialModel -> ModelExpr
addCoes d = foldr1 addRe $
            map(\x ->
                  express (x ^. coeff)
                  `mulRe`
                  nthderiv
                    (toInteger (x ^. degree))
                    (sy (qw (d ^. depVar)))
                    (d ^. indepVar)
               )
               (d ^. coefficients)

-- | Create a 'DifferentialModel' from a given indepVar ('UnitalChunk'), DepVar ('ModelExpr'),
-- | Coefficients ('[Expr]'), Constant ('Expr'), UID ('String'), term ('NP'), definition ('Sentence').
makeLinear :: UnitalChunk -> ConstrConcept -> [CoeffDeriv] -> Expr -> String -> NP -> Sentence -> DifferentialModel
makeLinear dmIndepVar dmDepVar dmCoeff dmConst dmID dmTerm dmDefn =
  Linear dmIndepVar dmDepVar dmCoeff dmConst (dccWDS dmID dmTerm dmDefn)
