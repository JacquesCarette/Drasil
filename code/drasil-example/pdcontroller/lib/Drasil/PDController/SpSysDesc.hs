module Drasil.PDController.SpSysDesc (goals, sysGoalInput, sysParts) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import Language.Drasil.Document (ConceptInstance, cic)

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.PDController.Concepts
import Drasil.PDController.Unitals (dqdPropGain, dqdDerivGain, dqdSetPointTD, dqdProcessVariableTD)

-- Introduction of the Problem Description section derives from purpose in
-- System (purp in Body.hs)

sysParts :: [Sentence]
sysParts
  = (!.) . D.toSent . atStartNP . the
      <$> [summingPt, pidC, powerPlant]

sysGoalInput :: [Sentence]
sysGoalInput
  = [phrase dqdSetPointTD, phrase simulationTime, phrase dqdPropGain, phrase dqdDerivGain,
     phrase stepTime]

goals :: [ConceptInstance]
goals = [sysProcessVariable]

sysProcessVariable :: ConceptInstance
sysProcessVariable
  = cic "calculateProcessVariable"
      (foldlSent
         [S "Calculate the output of the", phrase powerPlant,
            sParen (phrase dqdProcessVariableTD),
            S "over time"])
      "Process-Variable"
      goalStmtDom
