module Drasil.PDController.Unitals where

import Drasil.PDController.Concepts
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (second)
import Language.Drasil


syms, symFS, symFt, symnegInf, symposInf, syminvLaplace, symKd, symKp,
       symYT, symYS, symYrT, symYrS, symET, symES, symPS, symDS, symHS,
       symCT, symCS, symTStep, symTSim, symAbsTol, symRelTol,
       symDampingCoeff, symStifnessCoeff :: Symbol

symnegInf = Variable "-∞"
symposInf = Variable "∞"
symFS = sub (Variable "F") $ Label "s"
syminvLaplace = Variable "L⁻¹[F(s)]"
syms = Variable "s"
symFt = sub (Variable "f") $ Label "t"
symKd = sub (Variable "K") $ Label "d"
symKp = sub (Variable "K") $ Label "p"
symYrT = sub (Variable "r") $ Label "t"
symYrS = sub (Variable "R") $ Label "s"
symYT = sub (Variable "y") $ Label "t"
symYS = sub (Variable "Y") $ Label "s"
symET = sub (Variable "e") $ Label "t"
symES = sub (Variable "E") $ Label "s"
symPS = sub (Variable "P") $ Label "s"
symDS = sub (Variable "D") $ Label "s"
symHS = sub (Variable "H") $ Label "s"
symCT = sub (Variable "c") $ Label "t"
symCS = sub (Variable "C") $ Label "s"
symTStep = sub (Variable "t") $ Label "step"
symTSim = sub (Variable "t") $ Label "sim"
symAbsTol = Variable "AbsTol"
symRelTol = Variable "RelTol"
symDampingCoeff = Variable "c"
symStifnessCoeff = Variable "k"

symbols :: [QuantityDict]
symbols
  = [qdLaplaceTransform, qdFreqDomain, qdFxnTDomain, qdNegInf, qdPosInf,
     qdInvLaplaceTransform, qdPropGain, qdDerivGain, qdSetPointTD, qdSetPointFD,
     qdProcessVariableTD, qdProcessVariableFD, qdProcessErrorTD,
     qdProcessErrorFD, qdDerivativeControlFD, qdPropControlFD,
     qdTransferFunctionFD, qdCtrlVarTD, qdCtrlVarFD, qdStepTime, qdSimTime,
     qdDampingCoeff, qdStiffnessCoeff]

qdLaplaceTransform, qdFreqDomain, qdFxnTDomain, qdNegInf, qdPosInf,
                    qdInvLaplaceTransform, qdPropGain, qdDerivGain,
                    qdSetPointTD, qdSetPointFD, qdProcessVariableTD,
                    qdProcessVariableFD, qdProcessErrorTD, qdProcessErrorFD,
                    qdPropControlFD, qdDerivativeControlFD,
                    qdTransferFunctionFD, qdCtrlVarFD, qdCtrlVarTD, qdStepTime,
                    qdSimTime, qdDampingCoeff, qdStiffnessCoeff :: QuantityDict

inputs :: [QuantityDict]
inputs = [qdSetPointTD, qdDerivGain, qdPropGain, qdStepTime, qdSimTime]

outputs :: [QuantityDict]
outputs = [qdProcessVariableTD]

inputsUC :: [UncertQ]
inputsUC
  = [ipSetPtUnc, ipPropGainUnc, ipDerGainUnc, ipStepTimeUnc, ipSimTimeUnc]

inpConstrained :: [ConstrConcept]
inpConstrained
  = [ipPropGain, ipDerivGain, ipSetPt, ipStepTime, ipSimTime, opProcessVariable]

ipPropGain, ipDerivGain, ipSetPt, ipStepTime, ipSimTime, opProcessVariable ::
            ConstrConcept

ipSetPtUnc, ipPropGainUnc, ipDerGainUnc, ipStepTimeUnc, ipSimTimeUnc :: UncertQ

ipPropGain
  = constrained' (dqdNoUnit propGain symKp Real) [gtZeroConstr] (dbl 20)
ipPropGainUnc = uq ipPropGain defaultUncrt
qdPropGain = qw ipPropGain

ipDerivGain
  = constrained' (dqdNoUnit derGain symKd Real) [physc $ UpFrom (Inc, 0)]
      (dbl 1)
ipDerGainUnc = uq ipDerivGain defaultUncrt
qdDerivGain = qw ipDerivGain

ipSetPt = constrained' (dqdNoUnit setPoint symYrT Real) [gtZeroConstr] (dbl 1)
ipSetPtUnc = uq ipSetPt defaultUncrt
qdSetPointTD = qw ipSetPt

ipStepTime
  = constrained' (dqd stepTime symTStep Real second)
      [physc $ Bounded (Inc, 0.01  ) (Exc, sy ipSimTime)]
      (dbl 0.01  )
ipStepTimeUnc = uq ipStepTime defaultUncrt
qdStepTime = qw ipStepTime

ipSimTime
  = constrained' (dqd simulationTime symTSim Real second)
      [physc $ Bounded (Inc, 1) (Inc, 60)]
      (dbl 10)
ipSimTimeUnc = uq ipSimTime defaultUncrt
qdSimTime = qw ipSimTime

odeAbsTolConst, odeRelTolConst :: QDefinition

dqdAbsTol, dqdRelTol :: DefinedQuantityDict

pidConstants :: [QDefinition]
pidConstants = [odeAbsTolConst, odeRelTolConst]

pidDqdConstants :: [DefinedQuantityDict]
pidDqdConstants = [dqdAbsTol, dqdRelTol]

dqdAbsTol = dqdNoUnit ccAbsTolerance symAbsTol Real
dqdRelTol = dqdNoUnit ccRelTolerance symRelTol Real

odeAbsTolConst = mkQuantDef dqdAbsTol (Dbl 1.0e-10)
odeRelTolConst = mkQuantDef dqdRelTol (Dbl 1.0e-10)

opProcessVariable
  = constrained' (dqdNoUnit processVariable symYT (Vect Rational))
      [gtZeroConstr]
      (dbl 1)
qdProcessVariableTD = qw opProcessVariable

qdSetPointFD
  = vc "qdSetPointFD" (nounPhraseSent (S "Set-Point in the frequency domain"))
      symYrS
      Real

qdProcessVariableFD
  = vc "qdProcessVariableFD"
      (nounPhraseSent (S "Process Variable in the frequency domain"))
      symYS
      Real

qdProcessErrorTD
  = vc "qdProcessErrorTD"
      (nounPhraseSent (S "Process Error in the time domain"))
      symET
      Real

qdProcessErrorFD
  = vc "qdProcessErrorFD"
      (nounPhraseSent (S "Process Error in the frequency domain"))
      symES
      Real

qdPropControlFD
  = vc "qdPropControlFD"
      (nounPhraseSent (S "Proportional control in the frequency domain"))
      symPS
      Real

qdDerivativeControlFD
  = vc "qdDerivativeControlFD"
      (nounPhraseSent (S "Derivative control in the frequency domain"))
      symDS
      Real

qdTransferFunctionFD
  = vc "qdTransferFunctionFD"
      (nounPhraseSent (S "Transfer Function in the frequency domain"))
      symHS
      Real

qdCtrlVarTD
  = vc "qdCtrlVarTD" (nounPhraseSent (S "Control Variable in the time domain"))
      symCT
      Real

qdCtrlVarFD
  = vc "qdCtrlVarFD"
      (nounPhraseSent (S "Control Variable in the frequency domain"))
      symCS
      Real

qdLaplaceTransform
  = vc "qLaplaceTransform"
      (nounPhraseSent (S "Laplace Transform of a function"))
      symFS
      Real
qdFreqDomain
  = vc "qFreqDomain" (nounPhraseSent (S "Complex frequency-domain parameter"))
      syms
      Real
qdFxnTDomain
  = vc "qdFxnTDomain" (nounPhraseSent (S "Function in the time domain")) symFt
      Real

qdNegInf
  = vc "qdNegInf" (nounPhraseSent (S "Negative Infinity")) symnegInf Real

qdPosInf = vc "qdPosInf" (nounPhraseSent (S "Infinity")) symposInf Real

qdInvLaplaceTransform
  = vc "qInvLaplaceTransform"
      (nounPhraseSent (S "Inverse Laplace Transform of a function"))
      syminvLaplace
      Real

qdDampingCoeff
  = vc "qdDampingCoeff" (nounPhraseSent (S "Damping coeffecient of the spring"))
      symDampingCoeff
      Real

qdStiffnessCoeff
  = mkQuant "qdTimeConst"
      (nounPhraseSent (S "Stifness coeffecient of the spring"))
      symStifnessCoeff
      Real
      (Just second)
      Nothing
