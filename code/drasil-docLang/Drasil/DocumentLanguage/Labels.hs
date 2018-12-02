{-# Language Rank2Types #-}
module Drasil.DocumentLanguage.Labels
  where

import Language.Drasil


goalStmt_label :: Label
goalStmt_label = mkLabelRALst "goalStmt" "goalStmt"

solution_label :: Label
solution_label = mkLabelRALst "solutionGM" "solutionGM"

characteristics_label :: Label
characteristics_label = mkLabelRALst "characteristicsBR" "characteristicsBR"

physSystDescription_label :: Label
physSystDescription_label = mkLabelRALst "physSystDescriptionBR" "physSystDescriptionBR"
