{-# LANGUAGE ConstraintKinds #-}

-- | Defining all the classes which represent knowledge-about-knowledge.
module Language.Drasil.Classes (
    -- * Classes
    -- ** Chunks
    NamedIdea(term)
  , Idea(getA)
  , CommonIdea(abrv)
  , Concept
  , Definition(defn)
  , ConceptDomain(cdom)
  , Quantity
  , HasUnitSymbol(usymb)
  , HasReasVal(reasVal)
  , Constrained(constraints)
  , Callable
  , IsArgumentName
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

import Drasil.Database.UID (UID)
import Drasil.Language.Idea (Idea(..), NamedIdea(..))

import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.UnitLang (UDefn, USymb)
import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.ExprClasses (Express(express))
import Language.Drasil.Space (HasSpace)
import Language.Drasil.Sentence (Sentence)

import Control.Lens (Lens')

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

-- TODO: conceptual type synonym?
-- | Concepts are 'Idea's with definitions and domains.
type Concept c = (Idea c, Definition c, ConceptDomain c)
-- TODO: Would the below make this a bit better to work with?
--        type Concept = forall c. (Idea c, Definition c, ConceptDomain c) => c

-- | CommonIdea is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
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

-- TODO: This looks like it should be moved into drasil-code/?-base, it doesn't seem to be used enough atm.
--       ...but, Dr. Carette also mentioned these are dubious, maybe we should remove it?
-- | Some chunks can be called like functions.
class (HasSymbol c) => Callable c

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

-- TODO: This should be moved to `drasil-code-base`.
-- | Members must have a named argument.
class (HasSymbol c) => IsArgumentName c where
