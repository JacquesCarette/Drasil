{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.IModel where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Utils.Drasil (weave)

import Data.Drasil.Quantities.Physics (time)

import Theory.Drasil (InstanceModel, im, qwC, newDEModel')

import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.DataDefs
import Drasil.PDController.GenDefs
import Drasil.PDController.References
import Drasil.PDController.TModel
import Drasil.PDController.Unitals

instanceModels :: [InstanceModel]
instanceModels = [imPD]

----------------------------------------------

imPD :: InstanceModel
imPD
  = im (newDEModel' imPDRC)
      [qwC dqdSetPointTD $ UpFrom (Exc, exactDbl 0), qwC dqdPropGain $ UpFrom (Exc, exactDbl 0),
       qwC dqdDerivGain $ UpFrom (Exc, exactDbl 0)]
      dqdProcessVariableTD
      [UpFrom (Exc, exactDbl 0)]
      [dRef abbasi2015, dRef johnson2008]
      (Just imDeriv)
      "pdEquationIM"
      []

imPDRC :: DifferentialModel
imPDRC
  = makeASingleDE
      time
      opProcessVariable
      lhs
      rhs
      "imPDRC"
      (nounPhraseSP "Computation of the Process Variable as a function of time")
      EmptyS
      where lhs = [exactDbl 1 $+ sy dqdDerivGain $** (opProcessVariable $^^ 1)]
                  $++ (exactDbl 1 $** (opProcessVariable $^^ 2))
                  $++ (exactDbl 20 $+ sy dqdPropGain $** (opProcessVariable $^^ 0))
            rhs = sy dqdSetPointTD $* sy dqdPropGain
      -- Matrix form:
      -- coeffs = [[exactDbl 1, exactDbl 1 $+ sy dqdDerivGain, exactDbl 20 $+ sy dqdPropGain]]
      -- unknowns = [2, 1, 0]
      -- constants = [sy dqdSetPointTD $* sy dqdPropGain]

imDeriv :: Derivation
imDeriv
  = mkDerivName (phrase processVariable)
      (weave [imDerivStmts, map eS imDerivEqns])

imDerivStmts :: [Sentence]
imDerivStmts = [derivStmt1, derivStmt2, derivStmt3, derivStmt4]

imDerivEqns :: [ModelExpr]
imDerivEqns = [derivEqn1, derivEqn2, derivEqn3, derivEqn4]

derivStmt1 :: Sentence
derivStmt1
  = foldlSent
      [D.toSent (atStartNP (the processVariable)), eS' dqdProcessVariableFD, S "in a", phrase pidCL +:+
         S "is the product" `S.ofThe` phrase processError, fromSource ddErrSig `sC`
         phrase controlVariable, fromSource ddCtrlVar `sC` EmptyS
         `S.andThe` phrase powerPlant, fromSource gdPowerPlant]

derivEqn1 :: ModelExpr
derivEqn1
  = sy dqdProcessVariableFD
      $= (sy dqdSetPointFD $- sy dqdProcessVariableFD)
      $* (sy dqdPropGain $+ (sy dqdDerivGain $* sy dqdFreqDomain))
      $* recip_ (square (sy dqdFreqDomain) $+ sy dqdFreqDomain $+ exactDbl 20)

derivStmt2 :: Sentence
derivStmt2 = (S "Substituting the values and rearranging the equation" !.)

derivEqn2 :: ModelExpr
derivEqn2
  = square (sy dqdFreqDomain) $* sy dqdProcessVariableFD
      $+ ((exactDbl 1 $+ sy dqdDerivGain) $* sy dqdProcessVariableFD $* sy dqdFreqDomain)
      $+ ((exactDbl 20 $+ sy dqdPropGain) $* sy dqdProcessVariableFD)
      $- (sy dqdSetPointFD $* sy dqdFreqDomain $* sy dqdDerivGain)
      $- (sy dqdSetPointFD $* sy dqdPropGain) $= exactDbl 0

derivStmt3 :: Sentence
derivStmt3
  = S "Computing the" +:+ phrase dqdInvLaplaceTransform +:+
     fromSource tmInvLaplace +:+. S "of the equation"

derivEqn3 :: ModelExpr
derivEqn3
  = deriv (deriv (sy dqdProcessVariableTD) time) time $+
      (((exactDbl 1 $+ sy dqdDerivGain) $* deriv (sy dqdProcessVariableTD) time)
      $+ ((exactDbl 20 $+ sy dqdPropGain) $* sy dqdProcessVariableTD))
      $- (sy dqdDerivGain $* deriv (sy dqdSetPointTD) time)
      $- (sy dqdSetPointTD $* sy dqdPropGain) $= exactDbl 0

derivStmt4 :: Sentence
derivStmt4
  = foldlSent_
      [D.toSent (atStartNP (the setPoint)), eS' dqdSetPointTD, S "is a step function and a constant" +:+.
         fromSource aSP,
       S "Therefore the",
         S "differential" `S.ofThe` S "set point" `S.is` S "zero. Hence the equation",
         S "reduces to"]

derivEqn4 :: ModelExpr
derivEqn4
  = deriv (deriv (sy dqdProcessVariableTD) time) time $+
      ((exactDbl 1 $+ sy dqdDerivGain) $* deriv (sy dqdProcessVariableTD) time)
      $+ ((exactDbl 20 $+ sy dqdPropGain) $* sy dqdProcessVariableTD)
      $- (sy dqdSetPointTD $* sy dqdPropGain) $= exactDbl 0
