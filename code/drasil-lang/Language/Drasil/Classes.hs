{-# Language TypeFamilies #-}
-- | Defining all the classes which represent knowledge-about-knowledge
module Language.Drasil.Classes (
    HasUID(uid), UID
  , NamedIdea(term)
  , Idea(getA)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Concept
  , HasShortName(shortname)
  , HasSymbol(symbol)
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , IsUnit(udefn, getUnits)
  , HasLabel(getLabel)
  , MayHaveLabel(getMaybeLabel)
  , IsLabel
  , UnitEq(uniteq)
  , HasReference(getReferences)
  , HasReference2(getReferences2) -- hack for now...
  , CommonIdea(abrv)
  , Constrained(constraints)
  , HasReasVal(reasVal)
  , ExprRelat(relat)
  , DefiningExpr(defnExpr)
  , HasDerivation(derivations)
  , HasAdditionalNotes(getNotes)
  , HasRefAddress(getRefAdd)
  , Quantity
  , UncertainQuantity(uncert)
  , HasFields(getFields)
  ) where

-- some classes are so 'core' that they are defined elswhere
-- also helps with cycles...
import Language.Drasil.Classes.Core

import Language.Drasil.Chunk.Constrained.Core (Constraint)
import Language.Drasil.Data.Citation (CiteField)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.UnitLang(UDefn, USymb)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.RefTypes (Reference)
import Language.Drasil.RefProg (Reference2)
import Language.Drasil.Space (Space)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

import Control.Lens (Lens')

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

-- Temporary hack to avoid loss of information
class HasAdditionalNotes c where
  getNotes :: Lens' c [Sentence]

class ConceptDomain c where
  -- | cdom provides (a 'Lens' to) the concept domain tags for a chunk
  cdom :: Lens' c [UID]
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- | Concepts are 'Idea's with definitions and domains
class (Idea c, Definition c, ConceptDomain c) => Concept c where

-- | HasSpace is anything which has a Space...
class HasSpace c where
  typ      :: Lens' c Space

class HasReference c where
  getReferences :: Lens' c [Reference]

class HasReference2 c where
  getReferences2 :: Lens' c [Reference2]

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
  getLabel      :: Lens' c Label
 
class MayHaveLabel c where
  getMaybeLabel :: c -> Maybe Label

-- IsLabel is associated with String rendering
class (HasLabel u, HasUID u) => IsLabel u where

-- | A Quantity is an 'Idea' with a 'Space' and a symbol.
-- In theory, it should also have MayHaveUnit, but that causes
-- all sorts of import cycles (or lost of orphans)
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class Quantity c => UncertainQuantity c where
  uncert :: Lens' c (Maybe Double)

-- | Citations have Fields
class HasFields c where
  getFields :: Lens' c [CiteField]

-----------------------------------------------------
-- Below are for units only
-- | Some chunks store a unit symbol
class HasUnitSymbol u where
   usymb :: Lens' u USymb

-- | Units are Ideas with a Definition which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit
class (Idea u, Definition u, HasUnitSymbol u) => IsUnit u where
   udefn :: Lens' u (Maybe UDefn)
   getUnits :: u -> [UID]
-- Investigate (TODO): is this really needed?
class UnitEq u where
   uniteq :: Lens' u UDefn

-- TODO : there is a design bug here not at all apparent from its definition; have to come back to it (Pull Request #532)
class ExprRelat c where
  relat :: Lens' c Expr

-- This is the 'correct' version of ExprRelat.
class DefiningExpr c where
  defnExpr :: Lens' c Expr

