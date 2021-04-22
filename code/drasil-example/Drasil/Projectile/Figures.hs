module Drasil.Projectile.Figures (figLaunch) where

import Language.Drasil
import Utils.Drasil (the)

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figLaunch :: LabelledContent
figLaunch = llcc (makeFigRef "Launch") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "Launch.jpg") 70
