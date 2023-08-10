module Drasil.GamePhysics.Goals (goals, linearGS, angularGS) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (time)

import Data.Drasil.Quantities.Math (orientation)
import Data.Drasil.Quantities.Physics (angularVelocity, position, velocity)

goals :: [ConceptInstance]
goals = [linearGS, angularGS]

linearGS :: ConceptInstance
linearGS = cic "linearGS" (goalStatementStruct [position, velocity])
  "Determine-Linear-Properties" goalStmtDom

angularGS :: ConceptInstance
angularGS = cic "angularGS" (goalStatementStruct [orientation, angularVelocity])
  "Determine-Angular-Properties" goalStmtDom

goalStatementStruct :: (NamedIdea a) => [a] -> Sentence
goalStatementStruct outputs = foldlSent
  [S "Determine their new", foldlList Comma List $ map plural outputs,
    S "over a period" `S.of_` phrase time]
