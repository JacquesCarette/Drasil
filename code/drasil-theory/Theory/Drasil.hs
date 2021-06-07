{- re-export many things to simplify external use -}
module Theory.Drasil (
  -- Classes
    HasInputs(..), HasOutput(..)
  -- DataDefinition
  , DataDefinition, dd, ddNoRefs, qdFromDD
  -- GenDefn
  , GenDefn
  , gd, gdNoRefs, gd', gdNoRefs'
  , getEqModQdsFromGd
  -- MultiDefn
  , MultiDefn, DefiningExpr
  , mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr
  -- ModelKinds
  , ModelKinds(..), getEqModQds
  -- InstanceModel
  , InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , im', imNoDeriv', imNoRefs', imNoDerivNoRefs'
  , qwUC, qwC, getEqModQdsFromIm
  -- Theory
  , Theory(..), TheoryModel, tm, tmNoRefs, tm', tmNoRefs'
) where

import Theory.Drasil.Classes (HasInputs(..), HasOutput(..))
import Theory.Drasil.DataDefinition (DataDefinition, dd, ddNoRefs, qdFromDD)
import Theory.Drasil.GenDefn
import Theory.Drasil.ModelKinds
import Theory.Drasil.InstanceModel
import Theory.Drasil.MultiDefn
import Theory.Drasil.Theory
