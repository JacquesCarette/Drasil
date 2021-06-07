{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
module Theory.Drasil.ModelKinds (ModelKinds(..),
  setMk, elimMk, lensMk, getEqModQds) where

import Control.Lens (makeLenses, set, lens, to, (^.), Setter', Getter, Lens')
import Data.Maybe (mapMaybe)

import Language.Drasil (ExprRelat(..), ConceptDomain(..), Definition(..),
  Idea(..), NamedIdea(..), RelationConcept, QDefinition, HasUID(..))
import Theory.Drasil.MultiDefn (MultiDefn)

-- | Models can be of different kinds: 
--
--     * 'EquationalModel's represent quantities that are calculated via a single definition/'QDefinition'.
--     * 'EquationalRealm's represent MultiDefns; quantities that may be calculated using any one of many 'DefiningExpr's (e.g., 'x = A = ... = Z')
--     * 'DEModel's represent differential equations as 'RelationConcept's
--     * 'OthModel's are placeholders for models. No new 'OthModel's should be created, they should be using one of the other kinds.
data ModelKinds = EquationalModel QDefinition
                | EquationalRealm MultiDefn
                | DEModel RelationConcept
                | OthModel RelationConcept

makeLenses ''ModelKinds

-- | Finds the 'UID' of the 'ModelKinds'.
instance HasUID        ModelKinds where uid      = lensMk uid uid uid
-- | Finds the term ('NP') of the 'ModelKinds'.
instance NamedIdea     ModelKinds where term     = lensMk term term term
-- | Finds the idea of the 'ModelKinds'.
instance Idea          ModelKinds where getA     = elimMk (to getA) (to getA) (to getA)
-- | Finds the definition of the 'ModelKinds'.
instance Definition    ModelKinds where defn     = lensMk defn defn defn
-- | Finds the domain of the 'ModelKinds'.
instance ConceptDomain ModelKinds where cdom     = elimMk (to cdom) (to cdom) (to cdom)
-- | Finds the relation expression of the 'ModelKinds'.
instance ExprRelat     ModelKinds where relat    = elimMk (to relat) (to relat) (to relat)

-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels & RelationConcepts (else we'd be breaking too much of `stable`)

-- | Retrieve internal data from ModelKinds
elimMk :: Getter QDefinition a -> Getter MultiDefn a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ _ (EquationalModel q) = q ^. l
elimMk _ l _ (EquationalRealm q) = q ^. l
elimMk _ _ l (DEModel q)         = q ^. l
elimMk _ _ l (OthModel q)        = q ^. l

-- | Map into internal representations of ModelKinds
setMk :: ModelKinds -> Setter' QDefinition a -> Setter' MultiDefn a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q) f _ _ x = EquationalModel $ set f x q
setMk (EquationalRealm q) _ f _ x = EquationalRealm $ set f x q
setMk (DEModel q)         _ _ g x = DEModel $ set g x q
setMk (OthModel q)        _ _ g x = OthModel $ set g x q

-- | Make a 'Lens' for 'ModelKinds'.
lensMk :: forall a. Lens' QDefinition a -> Lens' MultiDefn a -> Lens' RelationConcept a -> Lens' ModelKinds a
lensMk lq lqd lr = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lq lqd lr mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lq lqd lr x

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKinds] -> [QDefinition]
getEqModQds = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
