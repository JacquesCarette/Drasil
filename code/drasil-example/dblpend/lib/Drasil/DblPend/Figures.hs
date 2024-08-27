module Drasil.DblPend.Figures (figMotion, sysCtxFig1) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/dblpend/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "dblpend") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "dblpend.png") 60

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "SystemContextFigure.png")
