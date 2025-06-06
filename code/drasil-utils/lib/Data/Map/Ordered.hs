module Data.Map.Ordered (OrderedMap,
    null, size, empty, singleton,
    insert, lookup, elems, fromList) where

import Prelude hiding (lookup, null)

import qualified Data.Map as M
import Data.List (sortOn)
import Data.Foldable (Foldable(foldl'))

-- | Simple ordered map. Based on 'container's 'Data.Map'. Note: Deletions are
-- unsupported.
newtype OrderedMap k v = OM { im :: M.Map k (Int, v) }
  deriving (Show, Eq)

null :: OrderedMap k v -> Bool
null (OM m) = M.null m

size :: OrderedMap k v -> Int
size (OM m) = M.size m

empty :: OrderedMap k v
empty = OM M.empty

singleton :: k -> v -> OrderedMap k v
singleton k v = OM $ M.singleton k (0, v) -- Using 0 as the initial index

insert :: Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v (OM m) = OM $ M.insert k (M.size m, v) m -- Using M.size means that we need to be careful with deletions!!

lookup :: Ord k => k -> OrderedMap k v -> Maybe v
lookup k (OM m) = snd <$> M.lookup k m

elems :: OrderedMap k v -> [v]
elems (OM m) = map snd $ sortOn fst $ M.elems m

fromList :: Ord k => [(k, v)] -> OrderedMap k v
fromList []           = empty
fromList ((k, v):kvs) = foldl'(\acc (k', v') -> insert k' v' acc) (singleton k v) kvs -- Outermost entry is inserted first
