{-# Language TypeFamilies #-}
-- | Defining all the classes which represent knowledge-about-knowledge
module Language.Drasil.Classes (
    HasUID(uid), UID
  , NamedIdea(term)
  , Idea(getA)
  , Definition(defn)
  , ConceptDomain(cdom, DOM)
  , Concept
  , HasSymbol(symbol)
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , IsUnit(us)
  , UnitEq(uniteq)
  , HasAttributes(attributes)
  , CommonIdea(abrv)
  , Constrained(constraints)
  , HasReasVal(reasVal)
  ) where

import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Symbol (Stage,Symbol)
import Language.Drasil.Space (Space)
import Language.Drasil.UnitLang (USymb,UDefn)
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Constrained.Core (Constraint)
import Language.Drasil.Expr (Expr)


import Control.Lens (Lens')

type UID = String

-- | The most basic item: having a unique key, here a UID (as a String)
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
  type DOM c :: *
  -- | cdom provides (a 'Lens' to) the concept domain tags for a chunk
  cdom :: Lens' c [DOM c] 
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

-- | Anything with 'Attributes'
class HasAttributes c where
  attributes :: Lens' c Attributes

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

-----------------------------------------------------
-- Below are for units only
-- | Some chunks store a unit symbol
class HasUnitSymbol u where
   usymb :: Lens' u USymb

-- | Units are Ideas with a Definition which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit
class (Idea u, Definition u, HasUnitSymbol u) => IsUnit u where
   us :: Lens' u USymb

-- Investigate (TODO): is this really needed?
class UnitEq u where
   uniteq :: Lens' u UDefn

