module Drasil.PDController.LabelledContent (
    labelledContent, gsdSysContextFig, sysFigure
) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D

import Data.Drasil.Concepts.Documentation (sysCont, physicalSystem)

labelledContent :: [LabelledContent]
labelledContent = [gsdSysContextFig, sysFigure]

gsdSysContextFig :: LabelledContent
gsdSysContextFig
  = llcc (makeFigRef "systemContextDiag") $
      fig (titleize sysCont)
        "../../../../datafiles/pdcontroller/Fig_SystemContext.png"

sysFigure :: LabelledContent
sysFigure
  = llcc (makeFigRef "pidSysDiagram") $
      figWithWidth (D.toSent $ atStartNP $ the physicalSystem)
        "../../../../datafiles/pdcontroller/Fig_PDController.png"
        70
