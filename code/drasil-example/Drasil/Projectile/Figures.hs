module Drasil.Projectile.Figures (figLaunch) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figLaunch :: LabelledContent
figLaunch = llcc (makeFigRef "Launch") $ figWithWidth (S "The" +:+ phrase physicalSystem)
  (resourcePath ++ "Launch.jpg") 70
