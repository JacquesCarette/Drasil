{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}

module Theory.Drasil.Realms (Realm, RealmVariant, 
    mkRealm, mkRealmForQuant, mkRealmVariant) where

import Control.Lens ((^.), view, makeLenses, makeLensesFor)
import Data.List (union)
import qualified Data.List.NonEmpty as NE

import Language.Drasil

-- TODO: Need a system for instantiating Realms into QDefs via "selecting" variants

-- | 'RealmVariant's are partial components of QDefinitions, containing only
--   the defining expressions and descriptions.
--   Any 'RealmVariant' paired with a 'QuantityDict' will form a "whole" 'QDefinition'.
data RealmVariant = RV {
  _cd     :: [UID],    -- ^ Concept domain
  _rvDesc :: Sentence, -- ^ Defining description/statement
  _expr   :: Expr      -- ^ Defining expression
}
makeLensesFor [("_expr", "expr")] ''RealmVariant

-- | 'Realm's are QDefinition factories, used for showing one or more ways we
--   can define a QDefinition
data Realm = Realm {
    _rUid  :: UID,                     -- ^ UID
    _qd    :: QuantityDict,            -- ^ Underlying quantity it defines
    _rDesc :: Sentence,                -- ^ Defining description/statement
    _rvs   :: NE.NonEmpty RealmVariant -- ^ All possible/omitted ways we can define the related quantity
}
makeLenses ''Realm


instance HasUID        Realm where uid      = rUid
instance HasSymbol     Realm where symbol   = symbol . (^. qd)
instance NamedIdea     Realm where term     = qd . term
instance Idea          Realm where getA     = getA . (^. qd)
instance HasSpace      Realm where typ      = qd . typ
instance Quantity      Realm where
instance MayHaveUnit   Realm where getUnit  = getUnit . view qd
-- | The concept domain of a Realm is the union of the concept domains of the underlying variants.
instance ConceptDomain Realm where cdom     = foldr1 union . NE.toList . NE.map _cd . (^. rvs)
instance Definition    Realm where defn     = rDesc
-- | The related Relation of a Realm is defined as the quantity and the related expressions being equal
--   e.g., `q $= a $= b $= ... $= z`
instance ExprRelat     Realm where relat q  = sy q $= foldr1 ($=) (NE.map (^. expr) (q ^. rvs))

-- | Smart constructor for Realms, does nothing special at the moment
mkRealm :: UID -> QuantityDict -> Sentence -> NE.NonEmpty RealmVariant -> Realm
mkRealm = Realm

-- | Smart constructor for Realms defining UIDs using that of the QuantityDict
mkRealmForQuant :: QuantityDict -> Sentence -> NE.NonEmpty RealmVariant -> Realm
mkRealmForQuant q = mkRealm (q ^. uid) q

-- | Smart constructor for RealmVariants
mkRealmVariant :: [UID] -> Sentence -> Expr -> RealmVariant
mkRealmVariant = RV
