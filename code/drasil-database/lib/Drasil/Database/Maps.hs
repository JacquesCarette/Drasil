module Drasil.Database.Maps (invert) where

import Data.Map.Strict (Map, fromListWith, toList)

invert :: Ord v => Map k [v] -> Map v [k]
invert m = fromListWith (++) vks
  where
    kvs = toList m
    vks = [(v, [k]) | (k, vs) <- kvs, v <- vs]
