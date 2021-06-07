{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}

module Theory.Drasil.MultiDefn (MultiDefn, DefiningExpr, 
    mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr) where

import Control.Lens ((^.), view, makeLenses, makeLensesFor)
import Data.List (union)
import qualified Data.List.NonEmpty as NE

import Language.Drasil (($=), sy, QuantityDict, MayHaveUnit(..),
  ConceptDomain(..), Definition(..), ExprRelat(..), HasSpace(..),
  Idea(..), NamedIdea(..), Quantity, HasSymbol(..), HasUID(..),
  Expr, Sentence, UID)

-- TODO: Need a system for instantiating MultiDefns into QDefs via "selecting" variants

-- | A 'DefiningExpr' is the "Expr"-related components of a QDefinition.
data DefiningExpr = DefiningExpr {
  _cd     :: [UID],    -- ^ Concept domain
  _rvDesc :: Sentence, -- ^ Defining description/statement
  _expr   :: Expr      -- ^ Defining expression
}
makeLensesFor [("_expr", "expr")] ''DefiningExpr

-- | 'MultiDefn's are QDefinition factories, used for showing one or more ways we
--   can define a QDefinition
data MultiDefn = MultiDefn {
    _rUid  :: UID,                     -- ^ UID
    _qd    :: QuantityDict,            -- ^ Underlying quantity it defines
    _rDesc :: Sentence,                -- ^ Defining description/statement
    _rvs   :: NE.NonEmpty DefiningExpr -- ^ All possible/omitted ways we can define the related quantity
}
makeLenses ''MultiDefn


instance HasUID        MultiDefn where uid      = rUid
instance HasSymbol     MultiDefn where symbol   = symbol . (^. qd)
instance NamedIdea     MultiDefn where term     = qd . term
instance Idea          MultiDefn where getA     = getA . (^. qd)
instance HasSpace      MultiDefn where typ      = qd . typ
instance Quantity      MultiDefn where
instance MayHaveUnit   MultiDefn where getUnit  = getUnit . view qd
-- | The concept domain of a MultiDefn is the union of the concept domains of the underlying variants.
instance ConceptDomain MultiDefn where cdom     = foldr1 union . NE.toList . NE.map _cd . (^. rvs)
instance Definition    MultiDefn where defn     = rDesc
-- | The related Relation of a MultiDefn is defined as the quantity and the related expressions being equal
--   e.g., `q $= a $= b $= ... $= z`
instance ExprRelat     MultiDefn where relat q  = sy q $= foldr1 ($=) (NE.map (^. expr) (q ^. rvs))

-- | Smart constructor for MultiDefns, does nothing special at the moment
mkMultiDefn :: UID -> QuantityDict -> Sentence -> NE.NonEmpty DefiningExpr -> MultiDefn
mkMultiDefn = MultiDefn

-- | Smart constructor for MultiDefns defining UIDs using that of the QuantityDict
mkMultiDefnForQuant :: QuantityDict -> Sentence -> NE.NonEmpty DefiningExpr -> MultiDefn
mkMultiDefnForQuant q = mkMultiDefn (q ^. uid) q

-- | Smart constructor for DefiningExprs
mkDefiningExpr :: [UID] -> Sentence -> Expr -> DefiningExpr
mkDefiningExpr = DefiningExpr
