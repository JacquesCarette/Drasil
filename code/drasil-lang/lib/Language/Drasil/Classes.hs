{-# LANGUAGE ConstraintKinds #-}

-- | Defining all the classes which represent knowledge-about-knowledge.
module Language.Drasil.Classes (
    -- * Classes
    -- ** Chunks
    NamedIdea(term)
  , Idea(getA)
  , CommonIdea(abrv)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Concept
  , DrasilConcept(metaConcept)
  , Quantity
  , HasUnitSymbol(usymb)
  , HasReasVal(reasVal)
  , Constrained(constraints)
  , HasAdditionalNotes(getNotes)
    -- the unsorted rest
  , IsUnit(udefn, getUnits)
  , UnitEq(uniteq)
    -- ** Expr and expressions
  , Express(express)
  , DefiningExpr(defnExpr)
  ) where

-- some classes are so 'core' that they are defined elsewhere
-- also helps with cycles...
import Language.Drasil.Symbol (HasSymbol)

import Language.Drasil.Chunk.NamedIdea (Idea(..), NamedIdea(..))
import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.UnitLang (UDefn, USymb)
import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.ExprClasses (Express(express))
import Language.Drasil.Space (HasSpace)
import Language.Drasil.Sentence (Sentence)
import Drasil.Database.UID (UID)

import Control.Lens (Lens', Getter)

-- TODO: conceptual typeclass?
-- TODO: I was thinking of splitting QDefinitions into Definitions with 2 type variables
--       Can we change this name from "Definition" to anything else? "NaturalDefinition"?
-- | Defines a chunk.
class Definition c where
  -- | Provides (a 'Lens' to) the definition for a chunk.
  defn :: Lens' c Sentence

-- TODO: conceptual typeclass?
-- Temporary hack to avoid loss of information
-- | Records any additional notes needed to avoid losing information
class HasAdditionalNotes c where
  -- | Provides a 'Lens' to the notes.
  getNotes :: Lens' c [Sentence]

-- TODO: `drasil-database`-related typeclass? UIDs should be  moved to `drasil-database` too.
-- | Some concepts have a domain (related information encoded in 'UID's to other chunks).
class ConceptDomain c where
  -- | Provides Getter for the concept domain tags for a chunk
  cdom :: c -> [UID]
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- | Concepts are 'Idea's with definitions and domains.
type Concept c = (Idea c, Definition c, ConceptDomain c)

-- | What does this chunk type encode?
-- 
-- All chunk types in Drasil should instantiate this class.
class DrasilConcept c where
  -- | Provide the 'UID' to the 'ConceptChunk' that explains what 'c' is.
  metaConcept :: Getter c UID

-- | CommonIdea is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation. This is the main
-- distinction between getA and abrv, where getA may return Nothing,
-- while abrv will always return the abbreviation. 
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: c -> String

-- | The Constrained class is a 'Quantity' that has value constraints.
-- It does not enforce 'Quantity' at this point.
class Constrained c where
  -- | Provides a 'Lens' to the 'Constraint's.
  constraints :: Lens' c [ConstraintE]

-- | A 'Quantity' that could have a reasonable value.
class HasReasVal c where
  -- | Provides a 'Lens' to the possible reasonable value.
  reasVal     :: Lens' c (Maybe Expr)

-- | A Quantity is an 'Idea' with a 'Space' and a 'Symbol'.
-- In theory, it should also restrict to being a part of 'MayHaveUnit', but that causes
-- all sorts of import cycles (or lots of orphans).
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where

-- TODO: potential alternative design for "Quantity"?
--
--      type Quantity2 = forall c. (Idea c, HasSpace c, HasSymbol c) => c

--  An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
--
-- class Quantity c => UncertainQuantity c where
--   uncert :: Lens' c (Uncertainty)
--   replaced with HasUncertainty

-----------------------------------------------------
-- Below are for units only
-- | Some chunks store a unit symbol.
class HasUnitSymbol u where
  -- | Provides the ability to hold a unit symbol ('USymb').
  usymb :: u -> USymb

-- | Units are 'Idea's with a 'Definition' which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit.
class (Idea u, Definition u, HasUnitSymbol u) => IsUnit u where
  -- | May have a unit definition.
  udefn :: u -> Maybe UDefn
  -- | Holds units as a list of 'UID'.
  getUnits :: u -> [UID]

-- Investigate (TODO): is this really needed?
-- | Contains a 'UDefn'
class UnitEq u where
  -- | Provides the 'Lens' to a unit definition.
  uniteq :: Lens' u UDefn

-----------------------------------------------------

class DefiningExpr c where
  -- | Provides a 'Lens' to the expression.
  --   TODO: Well, technically, `e` doesn't need to be an "expression" of any sorts.
  --         It just needs to be _something_, and it would have approximately have same meaning.
  defnExpr :: Lens' (c e) e
