{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
module Theory.Drasil.ModelKinds (ModelKinds(..), RealmVariant(..),
  setMk, elimMk, lensMk, getEqModQds) where

import Control.Lens ((^.), to, lens, set, makeLenses, makeLensesFor,
  Getter, Lens', Setter')
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE

import Language.Drasil (($=), sy, Expr, RelationConcept,
  NamedIdea(..), HasUID(..), ExprRelat(..), ConceptDomain(..), Definition(..),
  Idea(..), DefiningExpr(..), UID, Sentence, QDefinition, QuantityDict)

-- TODO: How do we want to instantiate RealmVariants?
-- Maybe we should move it into it's own model entirely?
data RealmVariant = RV {
  _desc :: Sentence,
  _expr :: Expr
}

makeLensesFor [("_expr", "expr")] ''RealmVariant

data ModelKinds = EquationalModel QDefinition
                | EquationalRealm QuantityDict [UID] (NE.NonEmpty RealmVariant)
                | DEModel RelationConcept
                | OthModel RelationConcept

makeLenses ''ModelKinds

instance HasUID        ModelKinds where uid      = lensMk uid uid uid
instance NamedIdea     ModelKinds where term     = lensMk term term term
instance Idea          ModelKinds where getA     = elimMk (to getA) (to getA) (to getA)
instance Definition    ModelKinds where defn     = lensMk defn (error "ambiguous definition (defn) in EquationalRealm") defn
instance ConceptDomain ModelKinds where cdom     = elimMk (to cdom) (error "ambiguous concept domain for EquationalRealm") (to cdom)
instance DefiningExpr  ModelKinds where defnExpr = lensMk defnExpr (error "ambiguous defining expression (defnExpr) for EquationalRealm") defnExpr
instance ExprRelat     ModelKinds where 
  relat (EquationalRealm q _ rs) = sy q $= foldr1 ($=) (NE.map (^. expr) rs)
  relat (EquationalModel q)      = relat q
  relat (DEModel q)              = relat q
  relat (OthModel q)             = relat q
-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels (else we'd be breaking too much of `stable`)

-- | Retrieve internal data from QDefinitions/RelationConcepts
elimMk :: Getter QDefinition a -> Getter QuantityDict a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ _ (EquationalModel q)     = q ^. l
elimMk _ l _ (EquationalRealm q _ _) = q ^. l
elimMk _ _ l (DEModel q)             = q ^. l
elimMk _ _ l (OthModel q)            = q ^. l

-- | Map into internal QDefinitions/RelationConcepts
setMk :: ModelKinds -> Setter' QDefinition a -> Setter' QuantityDict a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q)      f _ _ x = EquationalModel $ set f x q
setMk (EquationalRealm q cd vs) _ f _ x = EquationalRealm (set f x q) cd vs
setMk (DEModel q)              _ _ g x = DEModel $ set g x q
setMk (OthModel q)             _ _ g x = OthModel $ set g x q

lensMk :: forall a. Lens' QDefinition a -> Lens' QuantityDict a -> Lens' RelationConcept a -> Lens' ModelKinds a
lensMk lq lqd lr = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lq lqd lr mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lq lqd lr x

getEqModQds :: [ModelKinds] -> [QDefinition]
getEqModQds = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
