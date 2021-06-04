{-# Language Rank2Types #-}
module Drasil.DocumentLanguage.Labels where

import Language.Drasil (Reference, makeLstRef)

-- | Makes a reference label for a goal statement.
goalStmtLabel :: Reference
goalStmtLabel = makeLstRef "goalStmt" "goalStmt"
-- | Makes a reference label for a soultion statement.
solutionLabel :: Reference
solutionLabel = makeLstRef "solution" "solution"
-- | Makes a reference label for a characteristics statement.
characteristicsLabel :: Reference
characteristicsLabel = makeLstRef "characteristics" "characteristics"
