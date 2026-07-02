module Drasil.SSP.Figures (figForceActing) where

import Language.Drasil
import Language.Drasil.Document
import Data.Drasil.Concepts.Physics (force)
import Drasil.SSP.Defs (slice)

figForceActing :: LabelledContent
figForceActing = llccFig "ForceDiagram" $
  fig (atStart' force +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

resourcePath :: String
resourcePath = "../../../../datafiles/ssp/"
