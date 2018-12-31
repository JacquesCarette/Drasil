{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables #-}
module Language.Drasil.Chunk.InstanceModel
  ( InstanceModel
  , im', im'', getEqMod
  , inCons, outCons, imOutput, imInputs -- FIXME, these should be done via lenses
  , Constraints
  ) where

import Data.Drasil.IdeaDicts (instanceMod)
import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Chunk.CommonIdea (prependAbrv)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.Quantity (QuantityDict)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd), HasSymbol(symbol))
import Language.Drasil.Classes.Document (HasCitation(getCitations))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Quantity, HasSpace(typ),
  HasDerivation(derivations),  HasAdditionalNotes(getNotes), ExprRelat(relat),
  CommonIdea(abrv), Definition(defn))
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Development.Unit (MayHaveUnit(getUnit))
import Language.Drasil.Expr (Relation)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName, shortname')

import Control.Lens (makeLenses, view, lens, (^.), set, Getter, Setter', Lens', to)
import Data.Maybe (mapMaybe)

type Inputs = [QuantityDict]
type Output = QuantityDict

-- All constraints in an InstanceModel are always 'Assumed' !
type Constraints = [Relation]

type OutputConstraints = Constraints
type InputConstraints  = Constraints

data ModelKinds =
    EquationalModel QDefinition
  | DEModel RelationConcept
  | OthModel RelationConcept

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _mk :: ModelKinds
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

elim_mk :: (Getter QDefinition a) -> (Getter RelationConcept a) -> ModelKinds -> a
elim_mk l _ (EquationalModel q) = q ^. l
elim_mk _ l (DEModel q)         = q ^. l
elim_mk _ l (OthModel q)        = q ^. l

set_mk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
set_mk (EquationalModel q) f _ x = EquationalModel $ set f x q
set_mk (DEModel q)         _ g x = DEModel $ set g x q
set_mk (OthModel q)        _ g x = OthModel $ set g x q

lens_mk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' InstanceModel a
lens_mk lq lr = lens g s
    where g :: InstanceModel -> a
          g im = elim_mk lq lr (im ^. mk)
          s :: InstanceModel -> a -> InstanceModel
          s im x = set mk (set_mk (im ^. mk) lq lr x) im

instance HasUID             InstanceModel where uid = lens_mk uid uid
instance NamedIdea          InstanceModel where term = lens_mk term term
instance Idea               InstanceModel where getA = elim_mk (to getA) (to getA) . view mk
instance Definition         InstanceModel where defn = lens_mk defn defn
-- instance ConceptDomain      InstanceModel where cdom = elim_mk (to cdom) (to cdom) . view mk
instance ExprRelat          InstanceModel where relat = elim_mk (to relat) (to relat) . view mk
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

-- | Smart constructor for instance models; no derivations
im' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Citation] -> String -> [Sentence] -> InstanceModel
im' rcon i ic o oc src lbe addNotes =
  IM (OthModel rcon) i ic o oc src [] (shortname' lbe) (prependAbrv instanceMod lbe) addNotes

-- | im but with everything defined
im'' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Citation] -> Derivation -> String -> [Sentence] -> InstanceModel
im'' rcon i ic o oc src der sn addNotes = 
  IM (OthModel rcon) i ic o oc src der (shortname' sn) (prependAbrv instanceMod sn) addNotes

-- | Get equational models from a list of instance models
getEqMod :: [InstanceModel] -> [QDefinition]
getEqMod = mapMaybe isEqMod . map (view mk)
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
