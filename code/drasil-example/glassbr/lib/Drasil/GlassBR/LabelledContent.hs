module Drasil.GlassBR.LabelledContent (
  figures,
  sysCtxFig, physSystFig, demandVsSDFig, dimlessloadVsARFig
) where

import Control.Lens((^.))

import Language.Drasil
import Language.Drasil.Document
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Development as D (toSent)
import qualified Language.Drasil.Sentence.Combinators as S (versus)

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

import Drasil.GlassBR.Concepts (demandq, stdOffDist)
import Drasil.GlassBR.Unitals (aspectRatio, charWeight, demand,
  dimlessLoad, lateralLoad, stressDistFac)

resourcePath :: String
resourcePath = "../../../../datafiles/glassbr/"

figures :: [LabelledContent]
figures = [sysCtxFig, physSystFig,demandVsSDFig, dimlessloadVsARFig]

sysCtxFig, physSystFig, demandVsSDFig, dimlessloadVsARFig :: LabelledContent

sysCtxFig = llccFig "sysCtxDiag" $
  fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png")

physSystFig = llccFig "physSystImage" $ figWithWidth
  (D.toSent $ atStartNP $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30

demandVsSDFig = llccFig "demandVSsod" $ fig ((demandq ^. defn) +:+
  sParen (ch demand) `S.versus` atStart stdOffDist +:+ sParen (short stdOffDist)
  `S.versus` atStart charWeight +:+ sParen (ch charWeight))
  (resourcePath ++ "ASTM_F2248-09.png")

dimlessloadVsARFig = llccFig "dimlessloadVSaspect" $ fig (S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `S.versus` titleize aspectRatio +:+ sParen (short aspectRatio)
  `S.versus` atStart stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png")
