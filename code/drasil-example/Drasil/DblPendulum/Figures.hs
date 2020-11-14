module Drasil.DblPendulum.Figures (figMotion) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/DblPendulum/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "PendulumMotion") $ figWithWidth (S "The" +:+ phrase physicalSystem)
  (resourcePath ++ "pendulum.png") 70

