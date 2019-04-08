{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.InstanceModel
  ( InstanceModel
  , im', im''
  , inCons, outCons, imOutput, imInputs -- FIXME, these should be done via lenses
  , Constraints
  ) where

import Data.Drasil.IdeaDicts (instanceMod)
import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Chunk.CommonIdea (prependAbrv)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.Quantity (QuantityDict)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd), HasSymbol(symbol))
import Language.Drasil.Classes.Document (HasCitation(getCitations))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Quantity, HasSpace(typ),
  HasDerivation(derivations),  HasAdditionalNotes(getNotes), ExprRelat(relat),
  ConceptDomain(cdom), CommonIdea(abrv), Definition(defn),
  Referable(refAdd, renderRef))
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Label.Type (LblType(RP), prepend)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName, shortname')

import Control.Lens (makeLenses, view)

type Inputs = [QuantityDict]
type Output = QuantityDict

-- All constraints in an InstanceModel are always 'Assumed' !
type Constraints = [Relation]

type OutputConstraints = Constraints
type InputConstraints  = Constraints

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _imOutput :: Output
                        , _outCons :: OutputConstraints
                        , _cit :: [Citation]
                        , _deri :: Derivation
                        ,  lb :: ShortName
                        ,  ra :: String
                        , _notes :: [Sentence]
                        }
makeLenses ''InstanceModel

instance HasUID             InstanceModel where uid = rc . uid
instance NamedIdea          InstanceModel where term = rc . term
instance Idea               InstanceModel where getA = getA . view rc
instance Definition         InstanceModel where defn = rc . defn
instance ConceptDomain      InstanceModel where cdom = cdom . view rc
instance ExprRelat          InstanceModel where relat = rc . relat
instance HasDerivation      InstanceModel where derivations = deri
instance HasCitation        InstanceModel where getCitations = cit
instance HasShortName       InstanceModel where shortname = lb
instance HasRefAddress      InstanceModel where getRefAdd = ra
instance HasAdditionalNotes InstanceModel where getNotes = notes
instance HasSymbol          InstanceModel where symbol = symbol . view imOutput -- ???
instance HasSpace           InstanceModel where typ = imOutput . typ
instance Quantity           InstanceModel where
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view imOutput
instance CommonIdea         InstanceModel where abrv _ = abrv instanceMod
instance Referable          InstanceModel where
  refAdd    i = getRefAdd i
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for instance models; no derivations
im' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Citation] -> String -> [Sentence] -> InstanceModel
im' rcon i ic o oc src lbe addNotes =
  IM rcon i ic o oc src [] (shortname' lbe) (prependAbrv instanceMod lbe) addNotes

-- | im but with everything defined
im'' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Citation] -> Derivation -> String -> [Sentence] -> InstanceModel
im'' rcon i ic o oc src der sn addNotes = 
  IM rcon i ic o oc src der (shortname' sn) (prependAbrv instanceMod sn) addNotes
