module Drasil.SglPendulum.Figures (figMotion, sysCtxFig1) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/sglpendulum/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "sglpendulum") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "sglpendulum.jpg") 70

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "SystemContextFigure.png")
