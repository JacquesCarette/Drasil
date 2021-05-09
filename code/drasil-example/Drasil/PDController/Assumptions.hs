module Drasil.PDController.Assumptions where

import Data.Drasil.Concepts.Documentation (assumpDom)

import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.SI_Units (kilogram)
import Drasil.PDController.Concepts
import Language.Drasil
import Utils.Drasil

assumptions :: [ConceptInstance]
assumptions
  = [aPwrPlant, aDecoupled, aSP, aExtDisturb, aInitialValue, aParallelEq,
     aUnfilteredDerivative, apwrPlantTxFnx, aMass, aDampingCoeff,
     aStiffnessCoeff]

aPwrPlant, aDecoupled, aSP, aExtDisturb, aInitialValue, aParallelEq,
           aUnfilteredDerivative, apwrPlantTxFnx, aMass, aDampingCoeff,
           aStiffnessCoeff :: ConceptInstance

aPwrPlant = cic "pwrPlant" pwrPlantDesc "Power plant" assumpDom

aDecoupled = cic "decoupled" aDecoupledDesc "Decoupled equation" assumpDom

aSP = cic "setPoint" aSPDesc "Set-Point" assumpDom

aExtDisturb
  = cic "externalDisturb" aExtDisturbDesc "External disturbance" assumpDom

aInitialValue = cic "initialValue" aInitialValueDesc "Initial Value" assumpDom

aParallelEq = cic "parallelEq" aParallelEqDesc "Parallel Equation" assumpDom

apwrPlantTxFnx
  = cic "pwrPlantTxFnx" apwrPlantTxFnxDesc "Transfer Function" assumpDom

aUnfilteredDerivative
  = cic "unfilteredDerivative" aUnfilteredDerivativeDesc "Unfiltered Derivative"
      assumpDom

aMass = cic "massSpring" aMassDesc "Spring Mass" assumpDom

aDampingCoeff
  = cic "dampingCoeffSpring" aDampingCoeffDesc "Spring Damping Coefficient"
      assumpDom

aStiffnessCoeff
  = cic "stiffnessCoeffSpring" aStiffnessCoeffDesc "Spring Stiffness Coefficient"
      assumpDom

pwrPlantDesc, aDecoupledDesc, aSPDesc, aExtDisturbDesc, aManualTuningDesc,
              aInitialValueDesc, aParallelEqDesc, apwrPlantTxFnxDesc,
              aUnfilteredDerivativeDesc, aMassDesc, aDampingCoeffDesc,
              aStiffnessCoeffDesc :: Sentence
pwrPlantDesc
  = foldlSent
      [S "The" +:+ phrase powerPlant +:+ S "and the Sensor are coupled" +:+
         S "as a single unit"]

apwrPlantTxFnxDesc
  = foldlSent
      [S "The combined" +:+ phrase powerPlant +:+ S "and Sensor " +:+
         sParen (makeRef2S aPwrPlant)         
         +:+ S " are characterized by a Second Order mass-spring-damper"
         +:+ S " System"]

aDecoupledDesc
  = foldlSent
      [S "The decoupled form of the" +:+ phrase pidC +:+
         S "equation used in this"
         +:+ phrase simulation]

aSPDesc
  = foldlSent
      [S "The" +:+ phrase setPoint +:+ S "is constant throughout the" +:+
         phrase simulation]

aExtDisturbDesc
  = foldlSent
      [S "There are no external disturbances to the" +:+ phrase powerPlant +:+
         S "during the"
         +:+ phrase simulation]

aManualTuningDesc
  = foldlSent
      [S "This model will be used for  manual tuning of the" +:+ phrase pidC]

aInitialValueDesc
  = foldlSent
      [S "The initial value of the" +:+ phrase processVariable +:+
         S "is assumed to be zero"]

aParallelEqDesc
  = foldlSent
      [S "The Parallel form of the equation is used for the" +:+ phrase pidC]

aUnfilteredDerivativeDesc
  = foldlSent
      [S "A pure derivative function is used for this simulation;",
       S "there are no filters applied"]

aMassDesc
  = foldlSent
      [S "The", phrase mass,
       S "of the spring in the mass-spring-damper system ",
       sParen (makeRef2S aPwrPlant),
       S " is assumed to be 1", 
       phrase kilogram]

aDampingCoeffDesc
  = foldlSent
      [S "The", phrase ccDampingCoeff,
       S "of the spring in the mass-spring-damper system ",
       sParen (makeRef2S aPwrPlant), 
       S " is assumed to be 1"]

aStiffnessCoeffDesc
  = foldlSent
      [S "The", phrase ccStiffCoeff,
       S " of the spring in the mass-spring-damper system ",
       sParen (makeRef2S aPwrPlant), 
       S ") is assumed to be 20"]
