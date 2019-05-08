module Drasil.GamePhysics.Goals (goals) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (time)
import Data.Drasil.SentenceStructures (foldlSent, foldlList, FoldType(List), 
  SepType(Comma))

import Drasil.GamePhysics.Unitals (inputSymbols, outputSymbols)

goals :: [ConceptInstance]
goals = [linearGS, angularGS]

linearGS :: ConceptInstance
linearGS = cic "linearGS" (goalStatementStruct (take 2 outputSymbols)
  (S "their new") EmptyS) "Determine-Linear-Properties" goalStmtDom

angularGS :: ConceptInstance
angularGS = cic "angularGS" (goalStatementStruct (drop 3 $ take 5 inputSymbols)
  (S "their new") EmptyS) "Determine-Angular-Properties" goalStmtDom

goalStatementStruct :: (NamedIdea a) => [a] -> Sentence -> Sentence -> Sentence
goalStatementStruct outputs condition1 condition2 = foldlSent
  [ S "Determine", condition1, listOfOutputs, S "over a period of",
  (phrase time), condition2]
  where listOfOutputs       = (foldlList Comma List $ map plural outputs)
