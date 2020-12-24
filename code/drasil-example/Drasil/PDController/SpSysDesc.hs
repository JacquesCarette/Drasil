module Drasil.PDController.SpSysDesc where

import Data.Drasil.Concepts.Documentation (goalStmtDom, physicalSystem)

import Drasil.PDController.Concepts
import Language.Drasil
import Utils.Drasil

sysProblemDesc :: Sentence
sysProblemDesc
  = foldlSent_
      [S "provide a model of a" +:+ phrase pidC +:+
         S " that can be used for the tuning of the gain constants before"
         +:+ S " the deployment of the controller"]

sysParts :: [Sentence]
sysParts
  = map foldlSent
      [[S "The", phrase summingPt], [S "The", phrase pidC],
       [S "The", phrase powerPlant]]

sysFigure :: LabelledContent
sysFigure
  = llcc (makeFigRef "pidSysDiagram") $
      figWithWidth (atStartNP $ the physicalSystem)
        "../../../datafiles/PDController/Fig_PDController.png"
        70

sysGoalInput :: [Sentence]
sysGoalInput
  = [phrase setPoint, phrase simulationTime, phrase propGain, phrase derGain,
     phrase stepTime]

goals :: [ConceptInstance]
goals = [sysProcessVariable]

sysProcessVariable :: ConceptInstance
sysProcessVariable
  = cic "processVariable"
      (foldlSent
         [S "Calculate the" +:+ S "output of the" +:+ phrase powerPlant +:+
            sParen (phrase processVariable)
            +:+ S " over time"])
      "Process-Variable"
      goalStmtDom

