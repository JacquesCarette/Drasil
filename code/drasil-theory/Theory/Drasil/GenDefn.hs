{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.GenDefn (GenDefn,
  gd, gdNoRefs, gd', gdNoRefs', getEqModQdsFromGd) where

import Language.Drasil
import Data.Drasil.TheoryConcepts (genDefn)
import Theory.Drasil.ModelKinds (ModelKinds(..), elimMk, setMk, getEqModQds)

import Control.Lens (makeLenses, view, lens, (^.), set, Lens', to)

-- | A general definition is a 'ModelKind' that may have units, a derivation,
-- references, a shortname, a reference address, and notes.
data GenDefn = GD { _gUid  :: UID
                  , _mk    :: ModelKinds
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Maybe Derivation
                  , _ref   :: [Reference]
                  , _sn    :: ShortName
                  , _ra    :: String -- RefAddr
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

-- | Make 'Lens' for a 'GenDefn' based on its 'ModelKinds'.
lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' GenDefn a
lensMk lq lr = lens g s
    where g :: GenDefn -> a
          g gd_ = elimMk lq lr (gd_ ^. mk)
          s :: GenDefn -> a -> GenDefn
          s gd_ x = set mk (setMk (gd_ ^. mk) lq lr x) gd_

-- | Finds the 'UID' of a 'GenDefn'.
instance HasUID             GenDefn where uid = gUid
-- | Finds the term ('NP') of the 'GenDefn'.
instance NamedIdea          GenDefn where term = lensMk term term
-- | Finds the idea contained in the 'GenDefn'.
instance Idea               GenDefn where getA = elimMk (to getA) (to getA) . view mk
-- | Finds the definition of the 'GenDefn'.
instance Definition         GenDefn where defn = lensMk defn defn
-- | Finds the domain of the 'GenDefn'.
instance ConceptDomain      GenDefn where cdom = elimMk (to cdom) (to cdom) . view mk
-- | Finds the relation expression for a 'GenDefn'.
instance ExprRelat          GenDefn where relat = elimMk (to relat) (to relat) . view mk
-- | Finds the derivation of the 'GenDefn'. May contain Nothing.
instance HasDerivation      GenDefn where derivations = deri
-- | Finds 'Reference's contained in the 'GenDefn'.
instance HasReference       GenDefn where getReferences = ref
-- | Finds the 'ShortName' of the 'GenDefn'.
instance HasShortName       GenDefn where shortname = view sn
-- | Finds the reference address of the 'GenDefn'.
instance HasRefAddress      GenDefn where getRefAdd = view ra
-- | Finds any additional notes for the 'GenDefn'.
instance HasAdditionalNotes GenDefn where getNotes = notes
-- | Finds the units of the 'GenDefn'.
instance MayHaveUnit        GenDefn where getUnit = gdUnit
-- | Finds the idea of a 'GenDefn' (abbreviation).
instance CommonIdea         GenDefn where abrv _ = abrv genDefn
-- | Finds the reference address of a 'GenDefn'.
instance Referable          GenDefn where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for general definitions.
gd :: IsUnit u => ModelKinds -> Maybe u ->
  Maybe Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd mkind = gd' (mkind ^. uid) mkind

-- | Smart constructor for general definitions with no references.
gdNoRefs :: (IsUnit u) => ModelKinds -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs mkind = gdNoRefs' (mkind ^. uid) mkind

-- | Smart constructor for general definitions.
gd' :: IsUnit u => UID -> ModelKinds -> Maybe u ->
  Maybe Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd' gid _     _   _     []   _  = error $ "Source field of " ++ gid ++ " is empty"
gd' gid mkind u derivs refs sn_ = 
  GD gid mkind (fmap unitWrapper u) derivs refs (shortname' sn_) (prependAbrv genDefn sn_)

-- | Smart constructor for general definitions with no references.
gdNoRefs' :: IsUnit u => UID -> ModelKinds -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs' gid mkind u derivs sn_ = 
  GD gid mkind (fmap unitWrapper u) derivs [] (shortname' sn_) (prependAbrv genDefn sn_)

-- | Grab all related 'QDefinitions' from a list of general definitions.
getEqModQdsFromGd :: [GenDefn] -> [QDefinition]
getEqModQdsFromGd = getEqModQds . map _mk
