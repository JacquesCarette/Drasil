module Drasil.SglPend.LabelledContent (
  labelledContent, figMotion, sysCtxFig1
) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

labelledContent :: [LabelledContent]
labelledContent = [figMotion, sysCtxFig1]

resourcePath :: String
resourcePath = "../../../../datafiles/sglpend/"

figMotion :: LabelledContent
figMotion = llccFig "sglpend" $ figWithWidth (D.toSent $ atStartNP (the physicalSystem))
  (resourcePath ++ "sglpend.jpg") 70

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llccFig "sysCtxDiag" $ fig (titleize sysCont)
  (resourcePath ++ "SystemContextFigure.png")
