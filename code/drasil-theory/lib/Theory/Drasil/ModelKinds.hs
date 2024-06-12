{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators, GADTs  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Defines types and functions for creating models.
module Theory.Drasil.ModelKinds (
  -- * Types
  ModelKind(..), ModelKinds(..),
  -- * Constructors
  newDEModel, deModel, equationalConstraints, equationalModel, equationalRealm, othModel,
  newDEModel', deModel', equationalConstraints', equationalModel', equationalRealm', othModel',
  equationalModelU, equationalModelN, equationalRealmU, equationalRealmN,
  -- * Lenses
  setMk, elimMk, lensMk,
  -- * Functions
  getEqModQds
  ) where

import Control.Lens (makeLenses, set, lens, to, (^.), Setter', Getter, Lens')
import Data.Maybe (mapMaybe)

import Language.Drasil (NamedIdea(..), NP, QDefinition, HasUID(..), Expr,
  RelationConcept, ConceptDomain(..), Definition(..), Idea(..), Express(..),
  UID, DifferentialModel, mkUid, nsUid, RequiresChecking(..), Space,
  HasSpace(typ), DefiningExpr(..))
import Theory.Drasil.ConstraintSet (ConstraintSet)
import Theory.Drasil.MultiDefn (MultiDefn)

-- | Models can be of different kinds: 
--
--     * 'NewDEModel's represent differential equations as 'DifferentialModel's
--     * 'DEModel's represent differential equations as 'RelationConcept's
--     * 'EquationalConstraint's represent invariants that will hold in a system of equations.
--     * 'EquationalModel's represent quantities that are calculated via a single definition/'QDefinition'.
--     * 'EquationalRealm's represent MultiDefns; quantities that may be calculated using any one of many 'DefiningExpr's (e.g., 'x = A = ... = Z')
--     * 'FunctionalModel's represent quantity-resulting function definitions.
--     * 'OthModel's are placeholders for models. No new 'OthModel's should be created, they should be using one of the other kinds.
data ModelKinds e where
  NewDEModel            :: DifferentialModel -> ModelKinds e
  -- TODO: Analyze all instances of DEModels, convert them to (new, where
  -- applicable) variants of NewDEModel, and get rid of this.
  DEModel               :: RelationConcept   -> ModelKinds e
  EquationalConstraints :: ConstraintSet e   -> ModelKinds e
  EquationalModel       :: QDefinition e     -> ModelKinds e
  EquationalRealm       :: MultiDefn e       -> ModelKinds e
  -- TODO: Remove OthModel after having removed all instances of it.
  OthModel              :: RelationConcept   -> ModelKinds e

-- | 'ModelKinds' carrier, used to carry commonly overwritten information from
-- the IMs/TMs/GDs.
data ModelKind e = MK {
  _mk     :: ModelKinds e,
  _mkUID  :: UID,
  _mkTerm :: NP
}

makeLenses ''ModelKind

modelNs :: UID -> UID
modelNs = nsUid "theory"

-- | Smart constructor for 'NewDEModel's
newDEModel :: String -> NP -> DifferentialModel -> ModelKind e
newDEModel u n dm = MK (NewDEModel dm) (mkUid u) n

-- | Smart constructor for 'NewDEModel's, deriving UID+Term from the 'DifferentialModel'
newDEModel' :: DifferentialModel -> ModelKind e
newDEModel' dm = MK (NewDEModel dm) (modelNs $ dm ^. uid) (dm ^. term)

-- | Smart constructor for 'DEModel's
deModel :: String -> NP -> RelationConcept -> ModelKind e
deModel u n rc = MK (DEModel rc) (mkUid u) n

-- | Smart constructor for 'DEModel's, deriving UID+Term from the 'RelationConcept'
deModel' :: RelationConcept -> ModelKind e
deModel' rc = MK (DEModel rc) (modelNs $ rc ^. uid) (rc ^. term)

-- | Smart constructor for 'EquationalConstraints'
equationalConstraints :: String -> NP -> ConstraintSet e -> ModelKind e
equationalConstraints u n qs = MK (EquationalConstraints qs) (mkUid u) n

-- | Smart constructor for 'EquationalConstraints', deriving UID+Term from the 'ConstraintSet'
equationalConstraints' :: ConstraintSet e -> ModelKind e
equationalConstraints' qs = MK (EquationalConstraints qs) (modelNs $ qs ^. uid) (qs ^. term)

-- | Smart constructor for 'EquationalModel's
equationalModel :: String -> NP -> QDefinition e -> ModelKind e
equationalModel u n qd = MK (EquationalModel qd) (mkUid u) n

-- | Smart constructor for 'EquationalModel's, deriving UID+Term from the 'QDefinition'
equationalModel' :: QDefinition e -> ModelKind e
equationalModel' qd = MK (EquationalModel qd) (modelNs $ qd ^. uid) (qd ^. term)

-- | Smart constructor for 'EquationalModel's, deriving Term from the 'QDefinition'
equationalModelU :: String -> QDefinition e -> ModelKind e
equationalModelU u qd = MK (EquationalModel qd) (mkUid u) (qd ^. term)

-- | Smart constructor for 'EquationalModel's, deriving UID from the 'QDefinition'
equationalModelN :: NP -> QDefinition e -> ModelKind e
equationalModelN n qd = MK (EquationalModel qd) (modelNs $ qd ^. uid) n

-- | Smart constructor for 'EquationalRealm's
equationalRealm :: String -> NP -> MultiDefn e -> ModelKind e
equationalRealm u n md = MK (EquationalRealm md) (mkUid u) n

-- | Smart constructor for 'EquationalRealm's, deriving UID+Term from the 'MultiDefn'
equationalRealm' :: MultiDefn e -> ModelKind e
equationalRealm' md = MK (EquationalRealm md) (modelNs $ md ^. uid) (md ^. term)

-- | Smart constructor for 'EquationalRealm's
equationalRealmU :: String -> MultiDefn e -> ModelKind e
equationalRealmU u md = MK (EquationalRealm md) (mkUid u) (md ^. term)

-- | Smart constructor for 'EquationalRealm's, deriving UID from the 'MultiDefn'
equationalRealmN :: NP -> MultiDefn e -> ModelKind e
equationalRealmN n md = MK (EquationalRealm md) (modelNs $ md ^. uid) n

-- | Smart constructor for 'OthModel's
othModel :: String -> NP -> RelationConcept -> ModelKind Expr
othModel u n rc = MK (OthModel rc) (mkUid u) n

-- | Smart constructor for 'OthModel's, deriving UID+Term from the 'RelationConcept'
othModel' :: RelationConcept -> ModelKind e
othModel' rc = MK (OthModel rc) (modelNs $ rc ^. uid) (rc ^. term)

-- | Finds the 'UID' of the 'ModelKinds'.
instance HasUID        (ModelKinds e) where uid     = lensMk uid uid uid uid uid
-- | Finds the term ('NP') of the 'ModelKinds'.
instance NamedIdea     (ModelKinds e) where term    = lensMk term term term term term
-- | Finds the idea of the 'ModelKinds'.
instance Idea          (ModelKinds e) where getA    = elimMk (to getA) (to getA) (to getA) (to getA) (to getA)
-- | Finds the definition of the 'ModelKinds'.
instance Definition    (ModelKinds e) where defn    = lensMk defn defn defn defn defn
-- | Finds the domain of the 'ModelKinds'.
instance ConceptDomain (ModelKinds e) where cdom    = elimMk (to cdom) (to cdom) (to cdom) (to cdom) (to cdom)
-- | Rewrites the underlying model using 'ModelExpr'
instance Express e => Express (ModelKinds e) where
  express = elimMk (to express) (to express) (to express) (to express) (to express)
-- | Expose all expressions that need to be type-checked for theories that need
--   expose 'Expr's.
instance RequiresChecking (ModelKinds Expr) Expr Space where
  requiredChecks (NewDEModel dm)            = requiredChecks dm
  requiredChecks (DEModel _)                = mempty
  requiredChecks (EquationalConstraints cs) = requiredChecks cs
  requiredChecks (EquationalModel qd)       = pure (qd ^. defnExpr, qd ^. typ)
  requiredChecks (EquationalRealm md)       = requiredChecks md
  requiredChecks (OthModel _)               = mempty

-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels & RelationConcepts (else we'd be breaking too much of `stable`)

-- | Finds the 'UID' of the 'ModelKind'.
instance HasUID        (ModelKind e) where uid     = mkUID
-- | Finds the term ('NP') of the 'ModelKind'.
instance NamedIdea     (ModelKind e) where term    = mkTerm
-- | Finds the idea of the 'ModelKind'.
instance Idea          (ModelKind e) where getA    = getA . (^. mk)
-- | Finds the definition of the 'ModelKind'.
instance Definition    (ModelKind e) where defn    = mk . defn
-- | Finds the domain of the 'ModelKind'.
instance ConceptDomain (ModelKind e) where cdom    = cdom . (^. mk)
-- | Rewrites the underlying model using 'ModelExpr'
instance Express e => Express (ModelKind e) where
  express = express . (^. mk)
-- | Expose all expressions that need to be type-checked for theories that need
--   expose 'Expr's.
instance RequiresChecking (ModelKind Expr) Expr Space where
  requiredChecks = requiredChecks . (^. mk)

-- | Retrieve internal data from ModelKinds
elimMk :: Getter DifferentialModel a 
  -> Getter RelationConcept a -> Getter (ConstraintSet e) a
  -> Getter (QDefinition e) a -> Getter (MultiDefn e) a
  -> ModelKinds e -> a
elimMk f _ _ _ _ (NewDEModel q)            = q ^. f
elimMk _ f _ _ _ (DEModel q)               = q ^. f
elimMk _ _ f _ _ (EquationalConstraints q) = q ^. f
elimMk _ _ _ f _ (EquationalModel q)       = q ^. f
elimMk _ _ _ _ f (EquationalRealm q)       = q ^. f
elimMk _ f _ _ _ (OthModel q)              = q ^. f

-- | Map into internal representations of ModelKinds
setMk :: ModelKinds e
  -> Setter' DifferentialModel a
  -> Setter' RelationConcept a -> Setter' (ConstraintSet e) a
  -> Setter' (QDefinition e) a -> Setter' (MultiDefn e) a
  -> a -> ModelKinds e
setMk (NewDEModel q)            f _ _ _ _ x = NewDEModel            $ set f x q
setMk (DEModel q)               _ f _ _ _ x = DEModel               $ set f x q
setMk (EquationalConstraints q) _ _ f _ _ x = EquationalConstraints $ set f x q
setMk (EquationalModel q)       _ _ _ f _ x = EquationalModel       $ set f x q
setMk (EquationalRealm q)       _ _ _ _ f x = EquationalRealm       $ set f x q
setMk (OthModel q)              _ f _ _ _ x = OthModel              $ set f x q

-- | Make a 'Lens' for 'ModelKinds'.
lensMk :: forall e a.
     Lens' DifferentialModel a
  -> Lens' RelationConcept a -> Lens' (ConstraintSet e) a 
  -> Lens' (QDefinition e) a -> Lens' (MultiDefn e) a
  -> Lens' (ModelKinds e) a
lensMk ld lr lcs lq lmd = lens g s
    where g :: ModelKinds e -> a
          g = elimMk ld lr lcs lq lmd
          s :: ModelKinds e -> a -> ModelKinds e
          s mk_ = setMk mk_ ld lr lcs lq lmd

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKind e] -> [QDefinition e]
getEqModQds = mapMaybe eqMod
  where
    eqMod (MK (EquationalModel f) _ _) = Just f
    eqMod _                            = Nothing
