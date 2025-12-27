module Drasil.Database.Maps where

import Data.Map.Strict

invert :: Ord v => Map k [v] -> Map v [k]
invert m = fromListWith (++) vks
  where
    kvs = toList m
    vks = [(v, [k]) | (k, vs) <- kvs, v <- vs]
