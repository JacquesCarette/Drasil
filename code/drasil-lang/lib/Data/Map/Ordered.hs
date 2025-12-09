{-# LANGUAGE DeriveFunctor #-}

-- | A simple ordered map wrapper.
--   It preserves insertion order (important for deterministic rendering of basis blades).
--   Deletions are unsupported — removing elements would break ordering.
--
--   Inspired by Drasil PR 4009, where ordering was needed for consistent rendering.
module Data.Map.Ordered (
    OrderedMap,
    null, size, empty, singleton,
    insert, lookup, elems, fromList, toList
  ) where

import Prelude hiding (lookup, null)
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Foldable (Foldable(foldl'))

-- | An 'OrderedMap' associates each key with a value and an insertion index.
-- The index ensures that 'elems' and 'toList' return items in insertion order.
newtype OrderedMap k v = OM { im :: M.Map k (Int, v) }
  deriving (Show, Eq, Functor)

-- | Check if the map is empty.
null :: OrderedMap k v -> Bool
null (OM m) = M.null m

-- | Get the size of the map.
size :: OrderedMap k v -> Int
size (OM m) = M.size m

-- | An empty ordered map.
empty :: OrderedMap k v
empty = OM M.empty

-- | Create a singleton ordered map.
singleton :: k -> v -> OrderedMap k v
singleton k v = OM $ M.singleton k (0, v)

-- | Insert a key-value pair while preserving insertion order.
-- If the key already exists, its value is replaced but its order is unchanged.
insert :: Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v (OM m) = OM $
  case M.lookup k m of
    Just (i, _) -> M.insert k (i, v) m
    Nothing     -> M.insert k (M.size m, v) m

-- | Lookup a value by key.
lookup :: Ord k => k -> OrderedMap k v -> Maybe v
lookup k (OM m) = snd <$> M.lookup k m

-- | Get elements in insertion order.
elems :: OrderedMap k v -> [v]
elems (OM m) = map snd $ sortOn fst $ M.elems m

-- | Create an ordered map from a list (in list order).
fromList :: Ord k => [(k, v)] -> OrderedMap k v
fromList = foldl' (\acc (k, v) -> insert k v acc) empty

-- | Convert to an ordered association list.
toList :: OrderedMap k v -> [(k, v)]
toList (OM m) = [(k, v) | (k, (_, v)) <- sortOn (fst . snd) $ M.toList m]