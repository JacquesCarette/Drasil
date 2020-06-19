{-# LANGUAGE TemplateHaskell #-}
module Theory.Drasil.InstanceModel
  ( InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , qwUC, qwC
  ) where

import Language.Drasil
import Theory.Drasil.Classes (HasInputs(inputs), HasOutput(..))
import Data.Drasil.IdeaDicts (inModel)

import Control.Lens ((^.), makeLenses, view, _1, _2)

type Input = (QuantityDict, Maybe (RealInterval Expr Expr))
type Inputs = [Input]
type Output = QuantityDict
type OutputConstraints = [RealInterval Expr Expr]

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _imOutput :: (Output, OutputConstraints)
                        , _ref :: [Reference]
                        , _deri :: Maybe Derivation
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
instance Quantity           InstanceModel where
instance CommonIdea         InstanceModel where abrv _ = abrv inModel
instance Referable          InstanceModel where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)
instance HasInputs          InstanceModel where
  inputs          = imInputs
instance HasOutput          InstanceModel where
  output          = imOutput . _1
  out_constraints = imOutput . _2
instance HasSymbol          InstanceModel where symbol = symbol . view output -- ???
instance HasSpace           InstanceModel where typ = output . typ
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view output

-- | Smart constructor for instance models with everything defined
im :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> [Reference] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
im rcon _  _ _  [] _  _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
im rcon i o oc r der sn = 
  IM rcon i (o, oc) r der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivation
imNoDeriv :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> [Reference] -> String -> [Sentence] -> InstanceModel
imNoDeriv rcon _  _ _ [] _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
imNoDeriv rcon i o oc r sn =
  IM rcon i (o, oc) r Nothing (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no references
imNoRefs :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs rcon i o oc der sn = 
  IM rcon i (o, oc) [] der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivations or references
imNoDerivNoRefs :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs rcon i o oc sn = 
  IM rcon i (o, oc) [] Nothing (shortname' sn) (prependAbrv inModel sn)

-- | For building a quantity with no constraint
qwUC :: (Quantity q, MayHaveUnit q) => q -> Input 
qwUC x = (qw x, Nothing)

-- | For building a quantity with a constraint
qwC :: (Quantity q, MayHaveUnit q) => q -> RealInterval Expr Expr -> Input 
qwC x y = (qw x, Just y)
