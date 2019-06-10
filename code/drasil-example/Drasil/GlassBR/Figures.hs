module Drasil.GlassBR.Figures where

import Control.Lens((^.))

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumption, item, physicalSystem,
  requirement, section_, sysCont, traceyMatrix)

import Data.Drasil.SentenceStructures (showingCxnBw)

import Drasil.GlassBR.Concepts (aR, stdOffDist)
import Drasil.GlassBR.Unitals (aspectRatio, charWeight, demand, demandq,
  dimlessLoad, lateralLoad, sD, stressDistFac)

resourcePath :: String
resourcePath = "../../../datafiles/GlassBR/"

sysCtxFig, physSystFig, traceItemSecsFig, traceReqsItemsFig, traceAssumpsOthersFig, demandVsSDFig, dimlessloadVsARFig :: LabelledContent

sysCtxFig = llcc (makeFigRef "sysCtxDiag") $ 
  fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png") 

physSystFig = llcc (makeFigRef "physSystImage") $ figWithWidth 
  (atStartNP $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30

traceItemSecsFig = llcc (makeFigRef "TraceyItemSecs") $ fig (showingCxnBw traceyMatrix $
  titleize' item +:+ S "of Different" +:+ titleize' section_)
  (resourcePath ++ "Trace.png")

traceReqsItemsFig = llcc (makeFigRef "TraceyReqsItems") $ fig (showingCxnBw traceyMatrix $
  titleize' requirement `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "RTrace.png")

traceAssumpsOthersFig = llcc (makeFigRef "TraceyAssumpsOthers") $ fig (showingCxnBw traceyMatrix $
  titleize' assumption `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "ATrace.png")

demandVsSDFig = llcc (makeFigRef "demandVSsod") $ fig ((demandq ^. defn) +:+
  sParen (ch demand) `sVersus` atStart sD +:+ sParen (getAcc stdOffDist)
  `sVersus` atStart charWeight +:+ sParen (ch charWeight))
  (resourcePath ++ "ASTM_F2248-09.png")

dimlessloadVsARFig = llcc (makeFigRef "dimlessloadVSaspect") $ fig (S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `sVersus` titleize aspectRatio +:+ sParen (getAcc aR)
  `sVersus` atStart stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png")