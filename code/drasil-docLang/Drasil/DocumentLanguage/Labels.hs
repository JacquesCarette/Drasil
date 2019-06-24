{-# Language Rank2Types #-}
module Drasil.DocumentLanguage.Labels where

import Language.Drasil (Reference, makeLstRef)

goalStmtLabel :: Reference
goalStmtLabel = makeLstRef "goalStmt" "goalStmt"

solutionLabel :: Reference
solutionLabel = makeLstRef "solution" "solution"

characteristicsLabel :: Reference
characteristicsLabel = makeLstRef "characteristics" "characteristics"
