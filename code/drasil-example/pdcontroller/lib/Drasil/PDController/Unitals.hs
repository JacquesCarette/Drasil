module Drasil.PDController.Unitals where

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (second)
import Language.Drasil
import Utils.Drasil.Concepts

import Drasil.PDController.Concepts

syms, symFS, symFt, symnegInf, symposInf, syminvLaplace, symKd, symKp,
       symYT, symYS, symYrT, symYrS, symET, symES, symPS, symDS, symHS,
       symCT, symCS, symTStep, symTSim, symAbsTol, symRelTol,
       symDampingCoeff, symStifnessCoeff :: Symbol

symnegInf        = variable "-∞"
symposInf        = variable "∞"
symFS            = sub (variable "F") $ label "s"
syminvLaplace    = variable "L⁻¹[F(s)]"
syms             = variable "s"
symFt            = sub (variable "f") $ label "t"
symKd            = sub (variable "K") $ label "d"
symKp            = sub (variable "K") $ label "p"
symYrT           = sub (variable "r") $ label "t"
symYrS           = sub (variable "R") $ label "s"
symYT            = sub (variable "y") $ label "t"
symYS            = sub (variable "Y") $ label "s"
symET            = sub (variable "e") $ label "t"
symES            = sub (variable "E") $ label "s"
symPS            = sub (variable "P") $ label "s"
symDS            = sub (variable "D") $ label "s"
symHS            = sub (variable "H") $ label "s"
symCT            = sub (variable "c") $ label "t"
symCS            = sub (variable "C") $ label "s"
symTStep         = sub (variable "t") $ label "step"
symTSim          = sub (variable "t") $ label "sim"
symAbsTol        = variable "AbsTol"
symRelTol        = variable "RelTol"
symDampingCoeff  = variable "c"
symStifnessCoeff = variable "k"

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
  = constrained' (dqdNoUnit propGain symKp Real) [gtZeroConstr] (exactDbl 20)
ipPropGainUnc = uq ipPropGain defaultUncrt
qdPropGain = qw ipPropGain

ipDerivGain
  = constrained' (dqdNoUnit derGain symKd Real) [physc $ UpFrom (Inc, exactDbl 0)]
      (exactDbl 1)
ipDerGainUnc = uq ipDerivGain defaultUncrt
qdDerivGain = qw ipDerivGain

ipSetPt = constrained' (dqdNoUnit setPoint symYrT Real) [gtZeroConstr] (exactDbl 1)
ipSetPtUnc = uq ipSetPt defaultUncrt
qdSetPointTD = qw ipSetPt

ipStepTime
  = constrained' (ucs' stepTime symTStep Real second)
      [physc $ Bounded (Inc, frac 1 100) (Exc, sy ipSimTime)]
      (dbl 0.01)
ipStepTimeUnc = uq ipStepTime defaultUncrt
qdStepTime = qw ipStepTime

ipSimTime
  = constrained' (ucs' simulationTime symTSim Real second)
      [physc $ Bounded (Inc, exactDbl 1) (Inc, exactDbl 60)]
      (exactDbl 10)
ipSimTimeUnc = uq ipSimTime defaultUncrt
qdSimTime = qw ipSimTime

odeAbsTolConst, odeRelTolConst :: QDefinition Expr

dqdAbsTol, dqdRelTol :: DefinedQuantityDict

pidConstants :: [QDefinition Expr]
pidConstants = [odeAbsTolConst, odeRelTolConst]

pidDqdConstants :: [DefinedQuantityDict]
pidDqdConstants = [dqdAbsTol, dqdRelTol]

dqdAbsTol = dqdNoUnit ccAbsTolerance symAbsTol Real
dqdRelTol = dqdNoUnit ccRelTolerance symRelTol Real

odeAbsTolConst = mkQuantDef dqdAbsTol (dbl 1.0e-10)
odeRelTolConst = mkQuantDef dqdRelTol (dbl 1.0e-10)

opProcessVariable
  = constrained' (dqdNoUnit processVariable symYT (Vect Rational))
      [gtZeroConstr]
      (exactDbl 1)
qdProcessVariableTD = qw opProcessVariable

qdSetPointFD
  = vc "qdSetPointFD" (setPoint `inThe` ccFrequencyDomain) symYrS Real

qdProcessVariableFD = vc "qdProcessVariableFD" (processVariable `inThe` ccFrequencyDomain) symYS Real

qdProcessErrorTD
  = vc "qdProcessErrorTD"
      (nounPhraseSent (S "Process Error in the time domain"))
      symET
      Real

qdProcessErrorFD = vc "qdProcessErrorFD" (processError `inThe` ccFrequencyDomain) symES Real

qdPropControlFD  = vc "qdPropControlFD" (propControl `inThe` ccFrequencyDomain) symPS Real

qdDerivativeControlFD = vc "qdDerivativeControlFD" (derControl  `inThe` ccFrequencyDomain) symDS Real

qdTransferFunctionFD = vc "qdTransferFunctionFD" (ccTransferFxn  `inThe` ccFrequencyDomain) symHS Real

qdCtrlVarTD
  = vc "qdCtrlVarTD" (nounPhraseSent (S "Control Variable in the time domain"))
      symCT
      Real

qdCtrlVarFD = vc "qdCtrlVarFD" (controlVariable `inThe` ccFrequencyDomain) symCS Real

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
  = vc "qdDampingCoeff" (nounPhraseSent (S "Damping coefficient of the spring"))
      symDampingCoeff
      Real

qdStiffnessCoeff
  = mkQuant "qdTimeConst"
      (nounPhraseSent (S "Stiffness coefficient of the spring"))
      symStifnessCoeff
      Real
      (Just second)
      Nothing
