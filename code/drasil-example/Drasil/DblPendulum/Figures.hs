module Drasil.DblPendulum.Figures (figMotion, figRefs) where

import Language.Drasil
import Utils.Drasil.Concepts

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/DblPendulum/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "pendulum") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "pendulum.jpg") 70

-- References --
figRefs :: [Reference]
figRefs = [ref figMotion]
