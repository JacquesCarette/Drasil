module Data.Drasil.Plurals where

import qualified Data.Map as Map

irregularPlurals :: Map.Map String String
irregularPlurals = Map.fromList [
  ("body"    , "bodies"),
  ("velocity", "velocities")
  ]
