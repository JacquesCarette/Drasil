{-# LANGUAGE TemplateHaskell #-}
module Theory.Drasil.InstanceModel
  ( InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , inCons, outCons, imOutput, imInputs -- FIXME, these should be done via lenses
  , Constraints
  ) where

import Data.Drasil.IdeaDicts (instanceMod)
import Language.Drasil

import Control.Lens ((^.), makeLenses, view)

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
                        , _ref :: [Reference]
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
instance HasReference       InstanceModel where getReferences = ref
instance HasShortName       InstanceModel where shortname = lb
instance HasRefAddress      InstanceModel where getRefAdd = ra
instance HasAdditionalNotes InstanceModel where getNotes = notes
instance HasSymbol          InstanceModel where symbol = symbol . view imOutput -- ???
instance HasSpace           InstanceModel where typ = imOutput . typ
instance Quantity           InstanceModel where
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view imOutput
instance CommonIdea         InstanceModel where abrv _ = abrv instanceMod
instance Referable          InstanceModel where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for instance models with everything defined
im :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Reference] -> Derivation -> String -> [Sentence] -> InstanceModel
im rcon _ _  _ _  [] _  _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
im rcon i ic o oc r der sn = 
  IM rcon i ic o oc r der (shortname' sn) (prependAbrv instanceMod sn)

-- | Smart constructor for instance models; no derivation
imNoDeriv :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Reference] -> String -> [Sentence] -> InstanceModel
imNoDeriv rcon _ _  _ _ [] _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
imNoDeriv rcon i ic o oc r sn =
  IM rcon i ic o oc r [] (shortname' sn) (prependAbrv instanceMod sn)

-- | Smart constructor for instance models; no references
imNoRefs :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs rcon i ic o oc der sn = 
  IM rcon i ic o oc [] der (shortname' sn) (prependAbrv instanceMod sn)

-- | Smart constructor for instance models; no derivations or references
imNoDerivNoRefs :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs rcon i ic o oc sn = 
  IM rcon i ic o oc [] [] (shortname' sn) (prependAbrv instanceMod sn)

