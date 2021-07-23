module Drasil.SglPendulum.Figures (figMotion) where

import Language.Drasil
import Utils.Drasil.Concepts

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/SglPendulum/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "pendulum") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "pendulum.jpg") 70
