module Utils.Drasil.Maps where

import Data.Map.Strict

-- FIXME: Secretly has an error? The '[v]' might need to be a 'Set v'
invert :: Ord v => Map k [v] -> Map v [k]
invert m = fromListWith (++) vks
  where
    kvs = toList m
    vks = [(v, [k]) | (k, vs) <- kvs, v <- vs]
