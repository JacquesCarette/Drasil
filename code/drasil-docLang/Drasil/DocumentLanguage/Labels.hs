{-# Language Rank2Types #-}
module Drasil.DocumentLanguage.Labels
  where

import Language.Drasil


goalStmtLabel :: Reference
goalStmtLabel = makeLstRef "goalStmt" "goalStmt"

solutionLabel :: Reference
solutionLabel = makeLstRef "solution" "solution"

characteristics_label :: Reference
characteristics_label = makeLstRef "characteristics" "characteristics"

physSystDescription_label :: Reference
physSystDescription_label = makeLstRef "physSystDescription" "physSystDescription"
