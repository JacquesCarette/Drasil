module Drasil.Projectile.Figures (figLaunch, figRefs) where

import Language.Drasil
import Utils.Drasil.Concepts (the)

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figLaunch :: LabelledContent
figLaunch = llcc (makeFigRef "Launch") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "Launch.jpg") 70

-- References --
figRefs :: [Reference]
figRefs = [rw figLaunch]
