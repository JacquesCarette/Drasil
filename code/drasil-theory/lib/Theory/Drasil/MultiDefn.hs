{-# LANGUAGE PostfixOperators, Rank2Types, ScopedTypeVariables, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-- | Defines types and functions for creating mult-definitions.
module Theory.Drasil.MultiDefn(
  -- * Types
  MultiDefn, DefiningExpr,
  -- * Constructors
  mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr,
  -- * Functions
  multiDefnGenQD, multiDefnGenQDByUID,
) where

import Control.Lens (makeLenses, view, (^.))
import Data.List (union)
import qualified Data.List.NonEmpty as NE

import Drasil.Database (UID, HasUID(..), mkUid, showUID, HasChunkRefs(..))
import Language.Drasil hiding (DefiningExpr)

-- | 'DefiningExpr' are the data that make up a (quantity) definition, namely
--   the description, the defining (rhs) expression and the context domain(s).
--   These are meant to be 'alternate' but equivalent definitions for a single
--   concept.
data DefiningExpr e = DefiningExpr
  { -- | UID
    _deUid :: UID,
    -- | Concept domain
    _cd :: [UID],
    -- | Defining description/statement
    _rvDesc :: Sentence,
    -- | Defining expression
    _expr :: e
  }

makeLenses ''DefiningExpr

instance Eq (DefiningExpr e) where a == b = a ^. uid == b ^. uid

instance HasUID (DefiningExpr e) where uid = deUid

instance ConceptDomain (DefiningExpr e) where cdom = (^. cd)

instance Definition (DefiningExpr e) where defn = rvDesc

-- | 'MultiDefn's are QDefinition factories, used for showing one or more ways
--   we can define a QDefinition.
data MultiDefn e = MultiDefn{
  -- | UID
  _rUid :: UID,
  -- | Underlying quantity it defines.
  _qd :: DefinedQuantityDict,
  -- | Explanation of the different ways we can define a quantity.
  _rDesc :: Sentence,
  -- | All possible ways we can define the related quantity.
  _rvs :: NE.NonEmpty (DefiningExpr e)
}

makeLenses ''MultiDefn

instance HasChunkRefs (MultiDefn e) where
  chunkRefs = const mempty -- FIXME: `chunkRefs` should actually collect the referenced chunks.

instance HasUID           (MultiDefn e) where uid     = rUid
instance HasSymbol        (MultiDefn e) where symbol  = symbol . (^. qd)
instance NamedIdea        (MultiDefn e) where term    = qd . term
instance Idea             (MultiDefn e) where getA    = getA . (^. qd)
instance HasSpace         (MultiDefn e) where typ     = qd . typ
instance Definition       (MultiDefn e) where defn    = rDesc
instance Quantity         (MultiDefn e)
instance MayHaveUnit      (MultiDefn e) where getUnit = getUnit . view qd
-- | The concept domain of a MultiDefn is the union of the concept domains of
-- the underlying variants.
instance ConceptDomain    (MultiDefn e) where
  cdom = foldr1 union . NE.toList . NE.map (^. cd) . (^. rvs)
instance RequiresChecking (MultiDefn Expr) Expr Space where
  requiredChecks md = map (\x -> (x ^. expr, md ^. typ)) $ NE.toList $ md ^. rvs

-- | The complete Relation of a MultiDefn is defined as the quantity and the
--   related expressions being equal (e.g., `q $= a $= b $= ... $= z`)
instance Express e => Express (MultiDefn e) where
  express q = equiv $ sy q : NE.toList (NE.map (express . (^. expr)) (q ^. rvs))

-- | Smart constructor for MultiDefns, does nothing special at the moment. First
-- argument is the 'String' to become a 'UID'.
mkMultiDefn :: String -> DefinedQuantityDict -> Sentence -> NE.NonEmpty (DefiningExpr e) -> MultiDefn e
mkMultiDefn u q s des
  | length des == dupsRemovedLen = MultiDefn (mkUid u) q s des
  | otherwise                    = error $
      "MultiDefn `" ++ u ++ "` created with non-unique list of expressions"
  where
    dupsRemovedLen = length $ NE.nub des

-- Should showUID be used here?

-- | Smart constructor for 'MultiDefn's defining 'UID's using that of the 'DefinedQuantityDict'.
mkMultiDefnForQuant :: DefinedQuantityDict -> Sentence -> NE.NonEmpty (DefiningExpr e) -> MultiDefn e
mkMultiDefnForQuant q = mkMultiDefn (showUID q) q

-- | Smart constructor for 'DefiningExpr's.
mkDefiningExpr :: String -> [UID] -> Sentence -> e -> DefiningExpr e
mkDefiningExpr u = DefiningExpr (mkUid u)

-- | Convert 'MultiDefn's into 'QDefinition's via a specific 'DefiningExpr'.
multiDefnGenQD :: MultiDefn e -> DefiningExpr e -> QDefinition e
multiDefnGenQD md de =
  mkQDefSt
    (md ^. qd . uid)
    (md ^. term)
    (md ^. defn)
    (symbol md)
    (md ^. typ)
    (getUnit md)
    (de ^. expr)

-- | Convert 'MultiDefn's into 'QDefinition's via a specific 'DefiningExpr' (by 'UID').
multiDefnGenQDByUID :: MultiDefn e -> UID -> QDefinition e
multiDefnGenQDByUID md u
  | length matches == 1 = multiDefnGenQD md matched
  | otherwise           = error $
    "Invalid defining expression `" ++ show u ++ "` for QDef creation in MultiDefn `" ++ show u ++ "`"
  where
    matches = NE.filter (\x -> x ^. uid == u) (md ^. rvs)
    matched = head matches
