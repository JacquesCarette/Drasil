module Drasil.PDController.Concepts where

import Data.Drasil.Concepts.Documentation
       (assumption, goalStmt, physSyst, requirement, refBy, refName, srs, typUnc)
import Data.Drasil.TheoryConcepts
import Language.Drasil

acronyms :: [CI]
acronyms
  = [assumption, dataDefn, genDefn, goalStmt, inModel, physSyst, requirement, refBy, 
     refName, srs, thModel, typUnc, pdControllerCI, proportionalCI, derivativeCI,
     integralCI, pidCI]

pdControllerApp, pdControllerCI, proportionalCI, derivativeCI, integralCI,
                     pidCI :: CI

pdControllerApp = commonIdeaWithDict "pdControllerApp" (pn "PD Controller")                    "PD Controller" []
pdControllerCI  = commonIdeaWithDict "pdControllerCI"  (pn "proportional derivative")          "PD"            []
proportionalCI  = commonIdeaWithDict "proportionalCI"  (pn "proportional")                     "P"             []
derivativeCI    = commonIdeaWithDict "derivativeCI"    (pn "derivative")                       "D"             []
integralCI      = commonIdeaWithDict "integralCI"      (pn "integral")                         "I"             []
pidCI           = commonIdeaWithDict "pidCI"           (pn "proportional integral derivative") "PID"           []

pidC, pidCL, summingPt, powerPlant, secondOrderSystem, processError,
      simulationTime, processVariable, setPoint, propGain, derGain, 
      propControl, derControl, simulation,ccFrequencyDomain, ccTimeDomain,
      ccLaplaceTransform, controlVariable, stepTime, ccAbsTolerance, 
      ccRelTolerance, ccTransferFxn, ccDampingCoeff, ccStiffCoeff :: ConceptChunk
pidCL
  = dcc "pdCtrlLoop" (cn' "PD Control Loop") ("Closed-Loop control " ++
        "system with PD Controller, Summing Point and Power Plant")

pidC
  = dcc "pdController" (cn' "PD Controller") 
        "Proportional-Derivative Controller"

summingPt
  = dcc "summingPoint" (cn' "Summing Point") ("Control block where " ++
        "the difference between the Set-Point and the Process Variable " ++
        "is computed")

powerPlant
  = dcc "powerPlant" (cn' "Power Plant") 
      "A second order system to be controlled"

secondOrderSystem
  = dcc "secondOrderSystem" (cn' "Second Order System") 
      ("A system whose input-output relationship is denoted by a second-order "
         ++ "differential equation")

processError
  = dcc "processError" (cn' "Process Error") 
      ("Input to the PID controller. Process Error is the difference between the "
         ++ "Set-Point and the Process Variable")

stepTime = dcc "stepTime" (nounPhraseSP "Step Time") "Simulation step time"

simulationTime
  = dcc "simulationTime" (cn' "Simulation Time") 
      "Total execution time of the PD simulation"

processVariable
  = dcc "processVariable" (cn' "Process Variable") 
      "The output value from the power plant"

controlVariable
  = dcc "controlVariable" (cn' "Control Variable") 
      "The Control Variable is the output of the PD controller"

setPoint
  = dcc "setPoint" (cn' "Set-Point") 
      ("The desired value that the control system must reach. This also knows "
         ++ "as the reference variable")

propGain
  = dcc "propGain" (cn' "Proportional Gain") 
      "Gain constant of the proportional controller"

derGain
  = dcc "derGain" (cn' "Derivative Gain") 
      "Gain constant of the derivative controller"

propControl
  = dcc "propControl" (cn' "Proportional control")
      ("A linear feedback control system where correction is applied to the controlled " ++
      "variable which is proportional to the difference between desired and measured values")

derControl
  = dcc "derControl" (cn' "Derivative control")
      ("Monitors the rate of change of the error signal and contributes a component " ++ 
      "of the output signal (proportional to a derivative of the error signal)")

simulation
  = dcc "simulation" (cn' "simulation") 
      "Simulation of the PD controller"

ccFrequencyDomain
  = dcc "frequencyDomain" (nounPhraseSP "frequency domain") 
      ("The analysis of mathematical functions in terms of frequency, instead "
         ++ "of time")

ccTimeDomain 
  = dcc "timeDomain" (nounPhraseSP "time domain")
      "The analysis of mathematical functions in terms of time"

ccLaplaceTransform
  = dcc "laplaceTransform" (cn' "Laplace transform") 
      ("An integral transform that converts a function of a real variable t " ++
         "(often time) to a function of a complex variable s (complex frequency)")

ccAbsTolerance
  = dcc "absoluteTolerance" (nounPhraseSP "Absolute Tolerance") 
      "Absolute tolerance for the integrator"

ccRelTolerance
  = dcc "relativeTolerance" (nounPhraseSP "Relative Tolerance") 
      "Relative tolerance for the integrator"

ccTransferFxn
  = dcc "transferFxn" (cn' "Transfer Function")
      ("The Transfer Function of a system is the ratio of the output to the input"
         ++ " functions in the frequency domain")

ccDampingCoeff
  = dcc "dampingCoeff" (nounPhraseSP "Damping Coefficient")
      "Quantity that characterizes a second order system's oscillatory response"

ccStiffCoeff
  = dcc "stiffnessCoeff" (nounPhraseSP "Stiffness Coefficient")
      "Quantity that characterizes a spring's stiffness"

concepts :: [IdeaDict]
concepts = map nw defs

defs :: [ConceptChunk]
defs
  = [pidCL, pidC, summingPt, powerPlant, secondOrderSystem, processError,
     simulationTime, processVariable, setPoint, propGain, derGain, propControl,
    derControl, ccFrequencyDomain, ccTimeDomain, ccLaplaceTransform, controlVariable, stepTime,
     ccAbsTolerance, ccRelTolerance, ccTransferFxn, ccDampingCoeff,
     ccStiffCoeff]


