{-# Language Rank2Types #-}
module Drasil.DocumentLanguage.Labels where

import Language.Drasil (Reference, makeLstRef, phrase, plural)
import Data.Drasil.Concepts.Documentation (goalStmt, solution, characteristic)

-- | Makes a reference label for a goal statement.
goalStmtLabel :: Reference
goalStmtLabel = makeLstRef "goalStmt" $ phrase goalStmt
-- | Makes a reference label for a soultion statement.
solutionLabel :: Reference
solutionLabel = makeLstRef "solution" $ phrase solution
-- | Makes a reference label for a characteristics statement.
characteristicsLabel :: Reference
characteristicsLabel = makeLstRef "characteristics" $ plural characteristic
