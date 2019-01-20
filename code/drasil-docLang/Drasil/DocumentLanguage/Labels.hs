{-# Language Rank2Types #-}
module Drasil.DocumentLanguage.Labels
  where

import Language.Drasil


goalStmt_label :: Reference
goalStmt_label = makeLstRef "goalStmt" "goalStmt"

solution_label :: Reference
solution_label = makeLstRef "solution" "solution"

characteristics_label :: Reference
characteristics_label = makeLstRef "characteristics" "characteristics"

physSystDescription_label :: Reference
physSystDescription_label = makeLstRef "physSystDescription" "physSystDescription"
