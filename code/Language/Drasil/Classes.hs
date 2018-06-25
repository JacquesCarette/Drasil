{-# Language TypeFamilies #-}
-- | Defining all the classes which represent knowledge-about-knowledge
module Language.Drasil.Classes (
    HasUID(uid), UID
  , NamedIdea(term)
  , Idea(getA)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Concept
  , HasSymbol(symbol)
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , IsUnit(udefn)
  , HasLabel(getLabel)
  , IsLabel
  , UnitEq(uniteq)
  , HasReference(getReferences)
  , CommonIdea(abrv)
  , Constrained(constraints)
  , HasReasVal(reasVal)
  , ExprRelat(relat)
  , HasDerivation(derivations)
  ) where

import Language.Drasil.Chunk.Constrained.Core (Constraint)
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Symbol (Stage, Symbol)
import Language.Drasil.UID (UID)
import Language.Drasil.UnitLang (UDefn, USymb)

import Control.Lens (Lens')

-- | The most basic item: having a unique key, here a UID
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use
  uid :: Lens' c UID

-- | A NamedIdea is a 'term' that we've identified (has an 'id') as 
-- being worthy of naming.
class HasUID c => NamedIdea c where
  -- | Lens to the term (a noun phrase)
  term :: Lens' c NP

-- | An |Idea| is the 'meet' of |NamedIdea| and |CommonIdea|.
-- In other words, it /may/ have an acronym/abbreviation.
class NamedIdea c => Idea c where
  getA :: c -> Maybe String
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

class Definition c where
  -- | defn provides (a 'Lens' to) the definition for a chunk
  defn :: Lens' c Sentence

class ConceptDomain c where
  -- | cdom provides (a 'Lens' to) the concept domain tags for a chunk
  cdom :: Lens' c [UID]
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- | Concepts are 'Idea's with definitions and domains
class (Idea c, Definition c, ConceptDomain c) => Concept c where

-- | A HasSymbol is anything which has a Symbol
class HasSymbol c where
  -- | Provides the Symbol --  for a particular stage of generation
  symbol  :: c -> Stage -> Symbol
  
-- | HasSpace is anything which has a Space...
class HasSpace c where
  typ      :: Lens' c Space

class HasReference c where
  getReferences :: Lens' c References

class HasDerivation c where
  derivations :: Lens' c Derivation

-- | CommonIdea is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: c -> String

-- | A Constrained is a 'Quantity' that has value constraints
-- but do not enforce Quantity at this point
class Constrained c where
  constraints :: Lens' c [Constraint]

-- | A HasReasVal is a 'Quantity' that could have a reasonable value
class HasReasVal c where
  reasVal     :: Lens' c (Maybe Expr)

-- | For those things which "have a label"
class HasLabel c where
  getLabel :: Lens' c Label

-- IsLabel is associated with String rendering
class (HasLabel u, HasUID u) => IsLabel u where

-----------------------------------------------------
-- Below are for units only
-- | Some chunks store a unit symbol
class HasUnitSymbol u where
   usymb :: Lens' u USymb

-- | Units are Ideas with a Definition which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit
class (Idea u, Definition u, HasUnitSymbol u) => IsUnit u where
   udefn :: Lens' u (Maybe UDefn)
-- Investigate (TODO): is this really needed?
class UnitEq u where
   uniteq :: Lens' u UDefn

-- TODO : there is a design bug here not at all apparent from its definition; have to come back to it (Pull Request #532)
class ExprRelat c where
  relat :: Lens' c Expr
