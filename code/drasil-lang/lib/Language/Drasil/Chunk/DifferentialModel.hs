{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
    -- * Chunk Type
    DifferentialModel,
    -- * Constructors
    makeLinear
    ) where

import Control.Lens (makeLenses, (^.), view)-- set

import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS) -- cw
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Express(..), -- Concept,
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Expr.Lang (Expr(..))
-- u' = ax0 + bx1 + cx2 + .... + Contant -- 

-- 1 how to build the coefficients
-- 2 auto constr relation when coeff and const were given
-- 3 integrate ODEInfo, build ODEInfo
data DifferentialModel = Linear {
                                  -- _dmuid :: UID,
                                  -- _namedidea :: NP,
                                  -- _idea :: Maybe String,
                                  -- _defintion :: Sentence,
                                  -- _conceptdomain :: [UID],
                                  _coefficients :: [Expr], -- how this be built
                                  _constant :: Expr,
                                  _conc :: ConceptChunk,
                                  _rel  :: ModelExpr -- const from coeff and const
                                  -- ODEInfo
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
instance Express       DifferentialModel where express = (^. rel)

makeLinear :: Express e => String -> NP -> Sentence -> [Expr] -> Expr -> e -> DifferentialModel
makeLinear rID rTerm rDefn coeff consta = Linear coeff consta (dccWDS rID rTerm rDefn) . express
