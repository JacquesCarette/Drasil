
{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
module Theory.Drasil.ModelKinds (ModelKinds(..), mkEquatRealm, 
  setMk, elimMk, lensMk, getEqModQds) where

import Language.Drasil (QDefinition, RelationConcept, ConceptDomain(..),
  Definition(..), ExprRelat(..), Idea(..), NamedIdea(..), HasUID(..))
import Control.Lens (Lens', (^.), to, lens, set, makeLenses, Getter, Setter')
import Data.Maybe (mapMaybe)

data ModelKinds = EquationalModel QDefinition
                | EquationalRealm [QDefinition]
                -- | EquationalRealm' QuantityDict [(Sentence, Expr)]
                | DEModel RelationConcept
                | OthModel RelationConcept

makeLenses ''ModelKinds

instance HasUID        ModelKinds where uid   = lensMk uid uid
instance NamedIdea     ModelKinds where term  = lensMk term term
instance Idea          ModelKinds where getA  = elimMk (to getA) (to getA)
instance Definition    ModelKinds where defn  = lensMk defn defn
instance ConceptDomain ModelKinds where cdom  = elimMk (to cdom) (to cdom)
instance ExprRelat     ModelKinds where relat = elimMk (to relat) (to relat)

mkEquatRealm :: [QDefinition] -> ModelKinds
mkEquatRealm []  = error "EquationalRealms require at least 1 QDefinition"
mkEquatRealm qds = EquationalRealm qds

-- | Retrieve internal data from QDefinitions/RelationConcepts
elimMk :: Getter QDefinition a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ (EquationalModel q)  = q ^. l

elimMk _ _ (EquationalRealm []) = error "elimMk on empty EquationalRealm"
elimMk l _ (EquationalRealm qs) = head qs ^. l  
-- TODO: Is this really okay? It makes me think that we should have some sort of 
--   "QDefinitionModel" (QuantityDict?) and a list of pluggable 
--   tuples (defn, Expr) that can create a QDefinition with the QuantityDict

elimMk _ l (DEModel q)          = q ^. l
elimMk _ l (OthModel q)         = q ^. l

-- | Map into internal QDefinitions/RelationConcepts
setMk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q) f _ x = EquationalModel $ set f x q
setMk (EquationalRealm q) f _ x = EquationalRealm $ map (set f x) q
setMk (DEModel q)         _ g x = DEModel $ set g x q
setMk (OthModel q)        _ g x = OthModel $ set g x q

lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' ModelKinds a
lensMk lq lr = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lq lr mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lq lr x

-- TODO: Do we want to include EquationalRealms?
--       I don't think we do, they would result in QDefinitions with the same? UID but different Exprs 
getEqModQds :: [ModelKinds] -> [QDefinition]
getEqModQds = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
