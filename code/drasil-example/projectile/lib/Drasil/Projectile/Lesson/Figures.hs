module Drasil.Projectile.Lesson.Figures (figCSandA) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Development as D

import Data.Drasil.Concepts.Documentation (coordinateSystem)

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figCSandA :: LabelledContent
figCSandA = llcc (makeFigRef "CoordSystAndAssumpts") $ fig (D.toSent $ atStartNP $ the coordinateSystem)
  (resourcePath ++ "CoordSystAndAssumpts.png")
