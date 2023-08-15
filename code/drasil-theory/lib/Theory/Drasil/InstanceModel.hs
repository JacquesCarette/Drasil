{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, MultiParamTypeClasses #-}
-- | Defines types and functions for Instance Models.
module Theory.Drasil.InstanceModel(
  -- * Type
  InstanceModel
  -- * Constructors
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  -- * Functions
  , getEqModQdsFromIm
  , qwUC, qwC
  ) where

import Language.Drasil
import Language.Drasil.Development (showUID)
import Theory.Drasil.Classes (HasInputs(inputs), HasOutput(..))
import Data.Drasil.TheoryConcepts (inModel)

import Control.Lens ((^.), makeLenses, _1, _2) 
import Theory.Drasil.ModelKinds (ModelKind, getEqModQds)

type Input = (QuantityDict, Maybe (RealInterval Expr Expr))
type Inputs = [Input]
type Output = QuantityDict
type OutputConstraints = [RealInterval Expr Expr]

-- | An instance model is a ModelKind that may have specific inputs, outputs,
-- and output constraints. It also has attributes like references, derivation,
-- labels ('ShortName'), reference address, and notes.
data InstanceModel = IM {
    _mk       :: ModelKind Expr
  , _imInputs :: Inputs
  , _imOutput :: (Output, OutputConstraints)
  , _rf       :: [DecRef]
  , _deri     :: Maybe Derivation
  ,  lb       :: ShortName
  ,  ra       :: String
  , _notes    :: [Sentence]
}
makeLenses ''InstanceModel

-- | Finds the 'UID' of an 'InstanceModel'.
instance HasUID             InstanceModel where uid = mk . uid
-- | Finds the term ('NP') of the 'InstanceModel'.
instance NamedIdea          InstanceModel where term = mk . term
-- | Finds the idea contained in the 'InstanceModel'.
instance Idea               InstanceModel where getA = getA . (^. mk)
-- | Finds the definition of the 'InstanceModel'.
instance Definition         InstanceModel where defn = mk . defn
-- | Finds the domain of the 'InstanceModel'.
instance ConceptDomain      InstanceModel where cdom = cdom . (^. mk)
-- | Converts the 'InstanceModel's related expression into the display language.
instance Express            InstanceModel where express = express . (^. mk)
-- | Finds the derivation of the 'InstanceModel'. May contain Nothing.
instance HasDerivation      InstanceModel where derivations = deri
{--- | Finds 'Reference's contained in the 'InstanceModel'.
instance HasReference       InstanceModel where getReferences = rf-}
-- | Finds 'DecRef's contained in the 'InstanceModel'.
instance HasDecRef          InstanceModel where getDecRefs = rf
-- | Finds the 'ShortName' of the 'InstanceModel'.
instance HasShortName       InstanceModel where shortname = lb
-- | Finds the reference address of the 'InstanceModel'.
instance HasRefAddress      InstanceModel where getRefAdd l = RP (prepend $ abrv l) (ra l)
-- | Finds any additional notes for the 'InstanceModel'.
instance HasAdditionalNotes InstanceModel where getNotes = notes
-- | Finds the idea of an 'InstanceModel' (abbreviation).
instance CommonIdea         InstanceModel where abrv _ = abrv inModel
-- | Finds the reference address of an 'InstanceModel'.
instance Referable          InstanceModel where
  refAdd      = ra
  renderRef l = RP (prepend $ abrv l) (refAdd l)
-- | Finds the 'Quantity' of an 'InstanceModel'
instance DefinesQuantity    InstanceModel where
  defLhs = imOutput . _1
-- | Finds the inputs of an 'InstanceModel'.
instance HasInputs          InstanceModel where
  inputs          = imInputs
-- | Finds the outputs and output constraints of an 'InstanceModel'.
instance HasOutput          InstanceModel where
  output          = imOutput . _1
  out_constraints = imOutput . _2

-- | Expose all expressions that need to be type-checked.
instance RequiresChecking InstanceModel Expr Space where
  requiredChecks = requiredChecks . (^. mk)

-- | Smart constructor for instance models with everything defined.
im :: ModelKind Expr -> Inputs -> Output -> 
  OutputConstraints -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
im mkind _  _ _  [] _  _  = error $ "Source field of " ++ showUID mkind ++ " is empty"
im mkind i o oc r der sn = 
  IM mkind i (o, oc) r der (shortname' $ S sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models with a custom term, and no derivation.
imNoDeriv :: ModelKind Expr -> Inputs -> Output -> 
  OutputConstraints -> [DecRef] -> String -> [Sentence] -> InstanceModel
imNoDeriv mkind _ _ _  [] _  = error $ "Source field of " ++ showUID mkind ++ " is empty"
imNoDeriv mkind i o oc r  sn =
  IM mkind i (o, oc) r Nothing (shortname' $ S sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models with a custom term, and no references.
imNoRefs :: ModelKind Expr -> Inputs -> Output -> 
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs mkind i o oc der sn = 
  IM mkind i (o, oc) [] der (shortname' $ S sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models with a custom term, and no derivations or references.
imNoDerivNoRefs :: ModelKind Expr -> Inputs -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs mkind i o oc sn = 
  IM mkind i (o, oc) [] Nothing (shortname' $ S sn) (prependAbrv inModel sn)

-- | For building a quantity with no constraint.
qwUC :: (Quantity q, MayHaveUnit q) => q -> Input 
qwUC x = (qw x, Nothing)

-- | For building a quantity with a constraint.
qwC :: (Quantity q, MayHaveUnit q) => q -> RealInterval Expr Expr -> Input 
qwC x y = (qw x, Just y)

-- | Grab all related 'QDefinition's from a list of instance models.
getEqModQdsFromIm :: [InstanceModel] -> [SimpleQDef]
getEqModQdsFromIm ims = getEqModQds (map _mk ims)
