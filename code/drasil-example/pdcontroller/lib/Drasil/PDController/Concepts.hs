module Drasil.PDController.Concepts (
  acronyms, termDefs, defs, simulation, processError, simulationTime, stepTime,
  controlVariable, propControl, derControl,
  pdControllerCI, proportionalCI, pidC, piCI, pidCI, pidCL, processVariable,
  ccDampingCoeff, ccStiffCoeff, ccFrequencyDomain, ccLaplaceTransform, ccTransferFxn,
  ccAbsTolerance, ccRelTolerance,
  secondOrderSystem, summingPt, propGain, derGain, powerPlant, setPoint
) where

import Language.Drasil (commonIdeaWithDict, cn', nounPhraseSP, pn, CI, ConceptChunk,
  cncpt''', Sentence(..))
import Drasil.Database (mkUid)

acronyms :: [CI]
acronyms = [pdControllerCI, proportionalCI, piCI, pidCI]

pdControllerCI, proportionalCI, piCI, pidCI :: CI
pdControllerCI  = commonIdeaWithDict (mkUid "pdControllerCI")  (pn "proportional derivative")          "PD"            []
proportionalCI  = commonIdeaWithDict (mkUid "proportionalCI")  (pn "proportional")                     "P"             []
piCI            = commonIdeaWithDict (mkUid "piCI")            (pn "proportional integral")            "PI"            []
pidCI           = commonIdeaWithDict (mkUid "pidCI")           (pn "proportional integral derivative") "PID"           []

pidC, pidCL, summingPt, powerPlant, secondOrderSystem, processError,
      simulationTime, processVariable, setPoint, propGain, derGain,
      propControl, derControl, simulation,ccFrequencyDomain, ccTimeDomain,
      ccLaplaceTransform, controlVariable, stepTime, ccAbsTolerance,
      ccRelTolerance, ccTransferFxn, ccDampingCoeff, ccStiffCoeff :: ConceptChunk
pidCL
  = cncpt''' (mkUid "pdCtrlLoop") (nounPhraseSP "PD Control Loop") (S ("Closed-Loop control " ++
        "system with PD Controller, Summing Point and Power Plant"))

pidC
  = cncpt''' (mkUid "pdController") (nounPhraseSP "PD Controller")
        (S "Proportional-Derivative Controller")

summingPt
  = cncpt''' (mkUid "summingPoint") (nounPhraseSP "Summing Point") (S ("Control block where " ++
        "the difference between the Set-Point and the Process Variable " ++
        "is computed"))

powerPlant
  = cncpt''' (mkUid "powerPlant") (nounPhraseSP "Power Plant")
      (S "A second order system to be controlled")

secondOrderSystem
  = cncpt''' (mkUid "secondOrderSystem") (nounPhraseSP "Second Order System")
      (S ("A system whose input-output relationship is denoted by a second-order "
         ++ "differential equation"))

processError
  = cncpt''' (mkUid "processError") (nounPhraseSP "Process Error")
      (S ("Input to the PID controller. Process Error is the difference between the "
         ++ "Set-Point and the Process Variable"))

stepTime = cncpt''' (mkUid "stepTime") (nounPhraseSP "Step Time") (S "Simulation step time")

simulationTime
  = cncpt''' (mkUid "simulationTime") (nounPhraseSP "Simulation Time")
      (S "Total execution time of the PD simulation")

processVariable
  = cncpt''' (mkUid "processVariable") (nounPhraseSP "Process Variable")
      (S "The output value from the power plant")

controlVariable
  = cncpt''' (mkUid "controlVariable") (nounPhraseSP "Control Variable")
      (S "The Control Variable is the output of the PD controller")

setPoint
  = cncpt''' (mkUid "setPoint") (nounPhraseSP "Set-Point")
      (S ("The desired value that the control system must reach. This also knows "
         ++ "as the reference variable"))

propGain
  = cncpt''' (mkUid "propGain") (nounPhraseSP "Proportional Gain")
      (S "Gain constant of the proportional controller")

derGain
  = cncpt''' (mkUid "derGain") (nounPhraseSP "Derivative Gain")
      (S "Gain constant of the derivative controller")

propControl
  = cncpt''' (mkUid "propControl") (nounPhraseSP "Proportional control")
      (S ("A linear feedback control system where correction is applied to the controlled " ++
      "variable which is proportional to the difference between desired and measured values"))

derControl
  = cncpt''' (mkUid "derControl") (nounPhraseSP "Derivative control")
      (S ("Monitors the rate of change of the error signal and contributes a component " ++
      "of the output signal (proportional to a derivative of the error signal)"))

simulation
  = cncpt''' (mkUid "simulation") (cn' "simulation")
      (S "Simulation of the PD controller")

ccFrequencyDomain
  = cncpt''' (mkUid "frequencyDomain") (nounPhraseSP "frequency domain")
      (S ("The analysis of mathematical functions in terms of frequency, instead "
         ++ "of time"))

ccTimeDomain
  = cncpt''' (mkUid "timeDomain") (nounPhraseSP "time domain")
      (S "The analysis of mathematical functions in terms of time")

ccLaplaceTransform
  = cncpt''' (mkUid "laplaceTransform") (cn' "Laplace transform")
      (S ("An integral transform that converts a function of a real variable t " ++
         "(often time) to a function of a complex variable s (complex frequency)"))

ccAbsTolerance
  = cncpt''' (mkUid "absoluteTolerance") (nounPhraseSP "Absolute Tolerance")
      (S "Absolute tolerance for the integrator")

ccRelTolerance
  = cncpt''' (mkUid "relativeTolerance") (nounPhraseSP "Relative Tolerance")
      (S "Relative tolerance for the integrator")

ccTransferFxn
  = cncpt''' (mkUid "transferFxn") (nounPhraseSP "Transfer Function")
      (S ("The Transfer Function of a system is the ratio of the output to the input"
         ++ " functions in the frequency domain"))

ccDampingCoeff
  = cncpt''' (mkUid "dampingCoeff") (nounPhraseSP "Damping Coefficient")
      (S "Quantity that characterizes a second order system's oscillatory response")

ccStiffCoeff
  = cncpt''' (mkUid "ccStiffnessCoeff") (nounPhraseSP "Stiffness coefficient")
      (S "Quantity that characterizes a spring's stiffness")

defs :: [ConceptChunk]
defs
  = termDefs ++ [simulationTime, processVariable, setPoint, propGain, derGain,
     ccLaplaceTransform, stepTime,
     ccAbsTolerance, ccRelTolerance]

termDefs :: [ConceptChunk]
termDefs
  = [pidC, pidCL, summingPt, powerPlant, secondOrderSystem, processError,
     propControl, derControl, ccFrequencyDomain, ccTimeDomain,
     controlVariable, ccTransferFxn, ccDampingCoeff, ccStiffCoeff]
