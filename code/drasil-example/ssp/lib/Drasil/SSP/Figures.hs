module Drasil.SSP.Figures where

import Language.Drasil
import Data.Drasil.Concepts.Physics (force)
import Drasil.SSP.Defs (slice)

figForceActing :: LabelledContent
figForceActing = llccFig "ForceDiagram" $
  fig (atStart' force +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

resourcePath :: String
resourcePath = "../../../../datafiles/ssp/"
