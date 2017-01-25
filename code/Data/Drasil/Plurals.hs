module Data.Drasil.Plurals where

import qualified Data.Map as Map

pLook :: (Ord k) => k -> Map.Map k v -> Maybe v
pLook = Map.lookup

irregularPlurals :: Map.Map String String
irregularPlurals = Map.fromList [
  ("body", "bodies"),
  ("velocity", "velocities")
  ]