{- re-export many things to simplify external use -}
module Theory.Drasil (
  -- Classes
    HasInputs(..), HasOutput(..)
  -- ConstraintSet
  , ConstraintSet
  , mkConstraintSet
  -- DataDefinition
  , DataDefinition
  , dd, ddNoRefs, qdFromDD
  -- GenDefn
  , GenDefn
  , gd, gdNoRefs
  , getEqModQdsFromGd
  -- MultiDefn
  , MultiDefn, DefiningExpr
  , mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr
  , multiDefnGenQD, multiDefnGenQDByUID
  -- ModelKinds
  , ModelKind
  , deModel, equationalConstraints, equationalModel, equationalRealm, othModel
  , deModel', equationalConstraints', equationalModel', equationalRealm', othModel'
  , equationalModelU, equationalModelN, equationalRealmU, equationalRealmN
  -- InstanceModel
  , InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , qwUC, qwC, getEqModQdsFromIm
  -- Theory
  , Theory(..), TheoryModel
  , tm, tmNoRefs
) where

import Theory.Drasil.Classes (HasInputs(..), HasOutput(..))
import Theory.Drasil.ConstraintSet (ConstraintSet, mkConstraintSet)
import Theory.Drasil.DataDefinition (DataDefinition, dd, ddNoRefs, qdFromDD)
import Theory.Drasil.GenDefn (GenDefn, gd, gdNoRefs, getEqModQdsFromGd)
import Theory.Drasil.ModelKinds (ModelKind,
  deModel', deModel,
  equationalConstraints, equationalConstraints',
  equationalModel, equationalModel', equationalModelN, equationalModelU,
  equationalRealm, equationalRealm', equationalRealmN, equationalRealmU,
  othModel, othModel')
import Theory.Drasil.InstanceModel (InstanceModel,
  im, imNoDeriv, imNoRefs, imNoDerivNoRefs,
  qwUC, qwC,
  getEqModQdsFromIm)
import Theory.Drasil.MultiDefn (MultiDefn, DefiningExpr,
  mkDefiningExpr, mkMultiDefn,
  mkMultiDefnForQuant, multiDefnGenQD, multiDefnGenQDByUID)
import Theory.Drasil.Theory (tm, tmNoRefs, Theory(..), TheoryModel)