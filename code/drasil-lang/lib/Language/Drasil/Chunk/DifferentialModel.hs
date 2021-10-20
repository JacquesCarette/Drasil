{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
    -- * Chunk Type
    DifferentialModel,
    -- * Constructors
    makeLinear
    ) where

import Control.Lens (makeLenses, (^.), view)
import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Express(..),
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Chunk.Unital (UnitalChunk)
import Language.Drasil.ModelExpr.Class (ModelExprC(nthderiv, equiv))
import Language.Drasil.Expr.Class (ExprC(mulRe, addRe, exactDbl))

-- checked 1 how to build the coefficients 
-- checked 2 auto constr relation when coeff and const were given
-- 3 integrate ODEInfo, build ODEInfo
data DifferentialModel = Linear {
                                  _indepVar :: UnitalChunk, -- often time
                                  _depVar :: ModelExpr, -- qdProcessVariableTD in PDController
                                  _coefficients :: [Expr],
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
instance Express       DifferentialModel where express d = equiv $ displayODE d : [exactDbl 0]

-- | Construct a Canonical form of ODE, e.g. ax0 + bx1 + cx2 + .... + c
-- | x0 is the highest order, x1 is the second higest order, and so on. The c is the constant.
displayODE :: DifferentialModel -> ModelExpr
displayODE d = foldr1 addRe (
               zipWith mulRe (map express (d ^.coefficients))
               [nthderiv (toInteger n) (d ^. depVar) (d ^. indepVar)
               | n <- reverse [1..length (d ^. coefficients)]])
               `addRe` express (d ^. constant)

-- | Create a 'DifferentialModel' from a given indepVar ('UnitalChunk'), DepVar ('ModelExpr'),
-- | Coefficients ('[Expr]'), Constant ('Expr'), UID ('String'), term ('NP'), definition ('Sentence').
makeLinear :: UnitalChunk -> ModelExpr -> [Expr] -> Expr -> String -> NP -> Sentence -> DifferentialModel
makeLinear dmIndepVar dmDepVar dmCoeff dmConst dmID dmTerm dmDefn = 
  Linear dmIndepVar dmDepVar dmCoeff dmConst (dccWDS dmID dmTerm dmDefn)
