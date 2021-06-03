
{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.ModelKinds (ModelKinds(..), setMk, elimMk, lensMk, getEqModQds) where

import Language.Drasil (QDefinition, RelationConcept, ConceptDomain(..),
                        Definition(..), ExprRelat(..), Idea(..), 
                        NamedIdea(..), HasUID(..))
import Control.Lens (Lens', (^.), to, lens, set, makeLenses, Getter, Setter')
import Data.Maybe (mapMaybe)

-- | Models can be of different kinds: 
--
--     * Equational models contain 'QDefinition's,
--     * Differential equation models contain 'RelationConcept's,
--     * Other models contain 'RelationConcept's.
data ModelKinds = EquationalModel QDefinition
                | DEModel RelationConcept
                | OthModel RelationConcept

makeLenses ''ModelKinds

-- | Finds the 'UID' of the 'QDefinition' or 'RelationConcept' in the 'ModelKinds'.
instance HasUID             ModelKinds where uid = lensMk uid uid
-- | Finds the term ('NP') of the 'QDefinition' or 'RelationConcept' in the 'ModelKinds'.
instance NamedIdea          ModelKinds where term = lensMk term term
-- | Finds the idea of the 'QDefinition' or 'RelationConcept' contained in the 'ModelKinds'.
instance Idea               ModelKinds where getA = elimMk (to getA) (to getA)
-- | Finds the definition of the 'QDefinition' or 'RelationConcept' in the 'ModelKinds'.
instance Definition         ModelKinds where defn = lensMk defn defn
-- | Finds the domain of the 'QDefinition' or 'RelationConcept' in the 'ModelKinds'.
instance ConceptDomain      ModelKinds where cdom = elimMk (to cdom) (to cdom)
-- | Finds the relation expression for a 'QDefinition' or 'RelationConcept' contained in the 'ModelKinds'.
instance ExprRelat          ModelKinds where relat = elimMk (to relat) (to relat)

-- | A 'Getter' for extracting 'QDefinition's or 'RelationConcept's from 'ModelKinds'.
elimMk :: Getter QDefinition a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ (EquationalModel q) = q ^. l
elimMk _ l (DEModel q)         = q ^. l
elimMk _ l (OthModel q)        = q ^. l

-- | A 'Setter' for 'ModelKinds' built upon 'QDefinition's or 'RelationConcept's.
setMk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q) f _ x = EquationalModel $ set f x q
setMk (DEModel q)         _ g x = DEModel $ set g x q
setMk (OthModel q)        _ g x = OthModel $ set g x q

-- | Make a 'Lens' for 'ModelKinds'. They may contain either 'QDefinition's or 'RelationConcept's.
lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' ModelKinds a
lensMk lq lr = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lq lr mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lq lr x

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKinds] -> [QDefinition]
getEqModQds = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
