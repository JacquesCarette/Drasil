{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators, GADTs  #-}
module Theory.Drasil.ModelKinds (
    ModelKind(..), ModelKinds(..),
    deModel, equationalConstraints, equationalModel, equationalRealm, othModel,
    deModel', equationalConstraints', equationalModel', equationalRealm', othModel',
    equationalModelU, equationalModelN, equationalRealmU, equationalRealmN,
    setMk, elimMk, lensMk, getEqModQds
  ) where

import Control.Lens (makeLenses, set, lens, to, (^.), Setter', Getter, Lens')
import Data.Maybe (mapMaybe)

import Language.Drasil (NamedIdea(..), NP, QDefinition, HasUID(..), Expr,
  RelationConcept, ConceptDomain(..), Definition(..), Idea(..), Express(..), UID)
import Theory.Drasil.ConstraintSet (ConstraintSet)
import Theory.Drasil.MultiDefn (MultiDefn)

-- | Models can be of different kinds: 
--
--     * 'DEModel's represent differential equations as 'RelationConcept's
--     * 'EquationalConstraint's represent invariants that will hold in a system of equations.
--     * 'EquationalModel's represent quantities that are calculated via a single definition/'QDefinition'.
--     * 'EquationalRealm's represent MultiDefns; quantities that may be calculated using any one of many 'DefiningExpr's (e.g., 'x = A = ... = Z')
--     * 'FunctionalModel's represent quantity-resulting function definitions.
--     * 'OthModel's are placeholders for models. No new 'OthModel's should be created, they should be using one of the other kinds.
data ModelKinds e where
  DEModel               ::              RelationConcept -> ModelKinds Expr
  EquationalConstraints ::              ConstraintSet   -> ModelKinds Expr
  EquationalModel       :: Express e => QDefinition e   -> ModelKinds e
  EquationalRealm       :: Express e => MultiDefn e     -> ModelKinds e
  OthModel              ::              RelationConcept -> ModelKinds Expr

makeLenses ''ModelKinds

-- | 'ModelKinds' carrier, used to carry commonly overwritten information from the IMs/TMs/GDs.
data ModelKind e = MK {
  _mk     :: ModelKinds e,
  _mkUid  :: UID,
  _mkTerm :: NP
}

makeLenses ''ModelKind

-- TODO: RelationConcepts should contain ModelExprs instead of just Exprs

-- | Smart constructor for 'DEModel's
deModel :: UID -> NP -> RelationConcept -> ModelKind Expr
deModel u n rc = MK (DEModel rc) u n

-- | Smart constructor for 'DEModel's, deriving UID+Term from the 'RelationConcept'
deModel' :: RelationConcept -> ModelKind Expr
deModel' rc = MK (DEModel rc) (rc ^. uid) (rc ^. term)

-- | Smart constructor for 'EquationalConstraints'
equationalConstraints :: UID -> NP -> ConstraintSet-> ModelKind Expr
equationalConstraints u n qs = MK (EquationalConstraints qs) u n

-- | Smart constructor for 'EquationalConstraints', deriving UID+Term from the 'ConstraintSet'
equationalConstraints' :: ConstraintSet-> ModelKind Expr
equationalConstraints' qs = MK (EquationalConstraints qs) (qs ^. uid) (qs ^. term)

-- | Smart constructor for 'EquationalModel's
equationalModel :: Express e => UID -> NP -> QDefinition e -> ModelKind e
equationalModel u n qd = MK (EquationalModel qd) u n

-- | Smart constructor for 'EquationalModel's, deriving UID+Term from the 'QDefinition'
equationalModel' :: Express e => QDefinition e -> ModelKind e
equationalModel' qd = MK (EquationalModel qd) (qd ^. uid) (qd ^. term)

-- | Smart constructor for 'EquationalModel's, deriving Term from the 'QDefinition'
equationalModelU :: Express e => UID -> QDefinition e -> ModelKind e
equationalModelU u qd = MK (EquationalModel qd) u (qd ^. term)

-- | Smart constructor for 'EquationalModel's, deriving UID from the 'QDefinition'
equationalModelN :: Express e => NP -> QDefinition e -> ModelKind e
equationalModelN n qd = MK (EquationalModel qd) (qd ^. uid) n

-- | Smart constructor for 'EquationalRealm's
equationalRealm :: Express e => UID -> NP -> MultiDefn e -> ModelKind e
equationalRealm u n md = MK (EquationalRealm md) u n

-- | Smart constructor for 'EquationalRealm's, deriving UID+Term from the 'MultiDefn'
equationalRealm' :: Express e => MultiDefn e -> ModelKind e
equationalRealm' md = MK (EquationalRealm md) (md ^. uid) (md ^. term)

-- | Smart constructor for 'EquationalRealm's
equationalRealmU :: Express e => UID -> MultiDefn e -> ModelKind e
equationalRealmU u md = MK (EquationalRealm md) u (md ^. term)

-- | Smart constructor for 'EquationalRealm's, deriving UID from the 'MultiDefn'
equationalRealmN :: Express e => NP -> MultiDefn e -> ModelKind e
equationalRealmN n md = MK (EquationalRealm md) (md ^. uid) n

-- TODO: RelationConcepts should contain ModelExprs instead of just Exprs

-- | Smart constructor for 'OthModel's
othModel :: UID -> NP -> RelationConcept -> ModelKind Expr
othModel u n rc = MK (OthModel rc) u n

-- | Smart constructor for 'OthModel's, deriving UID+Term from the 'RelationConcept'
othModel' :: RelationConcept -> ModelKind Expr
othModel' rc = MK (OthModel rc) (rc ^. uid) (rc ^. term)

-- | Finds the 'UID' of the 'ModelKinds'.
instance Express e => HasUID        (ModelKinds e) where uid     = lensMk uid uid uid uid
-- | Finds the term ('NP') of the 'ModelKinds'.
instance Express e => NamedIdea     (ModelKinds e) where term    = lensMk term term term term
-- | Finds the idea of the 'ModelKinds'.
instance Express e => Idea          (ModelKinds e) where getA    = elimMk (to getA) (to getA) (to getA) (to getA)
-- | Finds the definition of the 'ModelKinds'.
instance Express e => Definition    (ModelKinds e) where defn    = lensMk defn defn defn defn
-- | Finds the domain of the 'ModelKinds'.
instance Express e => ConceptDomain (ModelKinds e) where cdom    = elimMk (to cdom) (to cdom) (to cdom) (to cdom)
-- | Rewrites the underlying model using 'ModelExpr'
instance Express e => Express       (ModelKinds e) where express = elimMk (to express) (to express) (to express) (to express)

-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels & RelationConcepts (else we'd be breaking too much of `stable`)

-- | Finds the 'UID' of the 'ModelKind'.
instance HasUID        (ModelKind e) where uid     = mkUid
-- | Finds the term ('NP') of the 'ModelKind'.
instance NamedIdea     (ModelKind e) where term    = mkTerm
-- | Finds the idea of the 'ModelKind'.
instance Express e => Idea          (ModelKind e) where getA    = getA . (^. mk)
-- | Finds the definition of the 'ModelKind'.
instance Express e => Definition    (ModelKind e) where defn    = mk . defn
-- | Finds the domain of the 'ModelKind'.
instance Express e => ConceptDomain (ModelKind e) where cdom    = cdom . (^. mk)
-- | Rewrites the underlying model using 'ModelExpr'
instance Express e => Express       (ModelKind e) where express = express . (^. mk)


-- | Retrieve internal data from ModelKinds
elimMk :: Getter RelationConcept a -> Getter ConstraintSet a
  -> Getter (QDefinition e) a -> Getter (MultiDefn e) a
  -> ModelKinds e -> a
elimMk f _ _ _ (DEModel q)               = q ^. f
elimMk _ f _ _ (EquationalConstraints q) = q ^. f
elimMk _ _ f _ (EquationalModel q)       = q ^. f
elimMk _ _ _ f (EquationalRealm q)       = q ^. f
elimMk f _ _ _ (OthModel q)              = q ^. f

-- | Map into internal representations of ModelKinds
setMk :: ModelKinds e
  -> Setter' RelationConcept a -> Setter' ConstraintSet a
  -> Setter' (QDefinition e) a -> Setter' (MultiDefn e) a
  -> a -> ModelKinds e
setMk (DEModel q)               f _ _ _ x = DEModel               $ set f x q
setMk (EquationalConstraints q) _ f _ _ x = EquationalConstraints $ set f x q
setMk (EquationalModel q)       _ _ f _ x = EquationalModel       $ set f x q
setMk (EquationalRealm q)       _ _ _ f x = EquationalRealm       $ set f x q
setMk (OthModel q)              f _ _ _ x = OthModel              $ set f x q

-- | Make a 'Lens' for 'ModelKinds'.
lensMk :: forall e a.
     Lens' RelationConcept a -> Lens' ConstraintSet a 
  -> Lens' (QDefinition e) a -> Lens' (MultiDefn e) a
  -> Lens' (ModelKinds e) a
lensMk lr lcs lq lmd = lens g s
    where g :: ModelKinds e -> a
          g mk_ = elimMk lr lcs lq lmd mk_
          s :: ModelKinds e -> a -> ModelKinds e
          s mk_ x = setMk mk_ lr lcs lq lmd x

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKind e] -> [QDefinition e]
getEqModQds = mapMaybe eqMod
  where
    eqMod (MK (EquationalModel f) _ _) = Just f
    eqMod _                            = Nothing
