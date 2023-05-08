module Language.Drasil.Chunk.ConstraintMap (ConstraintCEMap, ConstraintCE,
  constraintMap, physLookup, sfwrLookup
) where

import Control.Lens ((^.))

import Language.Drasil (Constraint, HasUID(..), UID, Constrained(..),
  isPhysC, isSfwrC)
import Language.Drasil.CodeExpr.Development (CodeExpr, constraint)
import qualified Data.Map as Map

-- | Type synonym for 'Constraint CodeExpr'
type ConstraintCE = Constraint CodeExpr

-- | Constraints map. Contains all 'Constraint's.
type ConstraintCEMap = Map.Map UID [ConstraintCE]

-- | Creates a map from 'UID' to 'Constraint's for constrained chunks.
constraintMap :: (HasUID c, Constrained c) => [c] -> ConstraintCEMap
constraintMap = Map.fromList . map (\x -> (x ^. uid, map constraint $ x ^. constraints))

-- | Returns a pair of a chunk and its physical constraints.
physLookup :: HasUID q => ConstraintCEMap -> q -> (q, [ConstraintCE])
physLookup m q = constraintLookup q m (filter isPhysC)

-- | Returns a pair of a chunk and its software constraints.
sfwrLookup :: HasUID q => ConstraintCEMap -> q -> (q, [ConstraintCE])
sfwrLookup m q = constraintLookup q m (filter isSfwrC)

-- | Returns a chunk and a filtered list of its constraints.
constraintLookup :: HasUID q => q -> ConstraintCEMap
                      -> ([ConstraintCE] -> [ConstraintCE]) -> (q, [ConstraintCE])
constraintLookup q m filt = (q, maybe [] filt (Map.lookup (q ^. uid) m))
