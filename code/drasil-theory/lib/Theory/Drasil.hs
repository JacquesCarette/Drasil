-- | Re-export many things to simplify external use.
module Theory.Drasil (
  -- * Classes
    HasInputs(..), HasOutput(..)
  -- * Constraint Sets
  , ConstraintSet
  , mkConstraintSet
  -- * Data Definitions
  , DataDefinition
  , ddE, ddENoRefs
  , ddME, ddMENoRefs
  , qdFromDD, qdEFromDD
  -- * General Definitions
  , GenDefn
  , gd, gdNoRefs
  , getEqModQdsFromGd
  -- * MultiDefn
  , MultiDefn, DefiningExpr
  , mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr
  , multiDefnGenQD, multiDefnGenQDByUID
  -- * ModelKinds
  , ModelKind
  , newDEModel, deModel, equationalConstraints, equationalModel, equationalRealm, othModel
  , newDEModel', deModel', equationalConstraints', equationalModel', equationalRealm', othModel'
  , equationalModelU, equationalModelN, equationalRealmU, equationalRealmN
  -- * Instance Models
  , InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , qwUC, qwC, getEqModQdsFromIm
  -- * Theory Models
  , Theory(..), TheoryModel
  , tm, tmNoRefs
) where

import Theory.Drasil.Classes (HasInputs(..), HasOutput(..))
import Theory.Drasil.ConstraintSet (ConstraintSet, mkConstraintSet)
import Theory.Drasil.DataDefinition (ddMENoRefs, ddME, ddENoRefs, ddE,
  DataDefinition, qdFromDD, qdEFromDD)
import Theory.Drasil.GenDefn (GenDefn, gd, gdNoRefs, getEqModQdsFromGd)
import Theory.Drasil.ModelKinds (ModelKind,
  newDEModel, newDEModel',
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
