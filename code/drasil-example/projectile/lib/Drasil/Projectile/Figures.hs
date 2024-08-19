module Drasil.Projectile.Figures (figLaunch, sysCtxFig1) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators (the)

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/projectile/"

figLaunch :: LabelledContent
figLaunch = llcc (makeFigRef "Launch") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "Launch.jpg") 70 WithCaption

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "SystemContextFigure.png") WithCaption
