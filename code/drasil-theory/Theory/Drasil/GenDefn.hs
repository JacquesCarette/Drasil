{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.GenDefn (GenDefn, gd, gdNoRefs, getEqModQdsFromGd) where

import Language.Drasil
import Data.Drasil.TheoryConcepts (genDefn)
import Theory.Drasil.ModelKinds (ModelKinds(..), elimMk, setMk, getEqModQds)

import Control.Lens (makeLenses, view, lens, (^.), set, Lens', to)

-- | A GenDefn is a ModelKind that may have units
data GenDefn = GD { _mk :: ModelKinds
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Maybe Derivation
                  , _ref   :: [Reference]
                  , _sn    :: ShortName
                  , _ra    :: String -- RefAddr
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' GenDefn a
lensMk lq lr = lens g s
    where g :: GenDefn -> a
          g gd_ = elimMk lq lr (gd_ ^. mk)
          s :: GenDefn -> a -> GenDefn
          s gd_ x = set mk (setMk (gd_ ^. mk) lq lr x) gd_

instance HasUID             GenDefn where uid = lensMk uid uid
instance NamedIdea          GenDefn where term = lensMk term term
instance Idea               GenDefn where getA = elimMk (to getA) (to getA) . view mk
instance Definition         GenDefn where defn = lensMk defn defn
instance ConceptDomain      GenDefn where cdom = elimMk (to cdom) (to cdom) . view mk
instance ExprRelat          GenDefn where relat = elimMk (to relat) (to relat) . view mk
instance HasDerivation      GenDefn where derivations = deri
instance HasReference       GenDefn where getReferences = ref
instance HasShortName       GenDefn where shortname = view sn
instance HasRefAddress      GenDefn where getRefAdd = view ra
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit = gdUnit
instance CommonIdea         GenDefn where abrv _ = abrv genDefn
instance Referable          GenDefn where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for general definitions
gd :: (IsUnit u) => ModelKinds -> Maybe u ->
  Maybe Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd mkind _   _     []   _  = error $ "Source field of " ++ mkind ^. uid ++ " is empty"
gd mkind u derivs refs sn_ = 
  GD mkind (fmap unitWrapper u) derivs refs (shortname' sn_) (prependAbrv genDefn sn_)

-- | Smart constructor for general definitions; no references
gdNoRefs :: (IsUnit u) => ModelKinds -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs mkind u derivs sn_ = 
  GD mkind (fmap unitWrapper u) derivs [] (shortname' sn_) (prependAbrv genDefn sn_)

-- | Grab all related QDefinitions from a list of general definitions
getEqModQdsFromGd :: [GenDefn] -> [QDefinition]
getEqModQdsFromGd gdefns = getEqModQds (map _mk gdefns)
