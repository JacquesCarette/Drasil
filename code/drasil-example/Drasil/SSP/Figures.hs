module Drasil.SSP.Figures where

import Language.Drasil
import Data.Drasil.Concepts.Physics (force)
import Drasil.SSP.Defs (slice)


fig_forceacting :: LabelledContent
fig_forceacting = llcc (mkLabelRAFig "ForceDiagram") $
  fig (at_start' force +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

resourcePath :: String
resourcePath = "../../../datafiles/SSP/"