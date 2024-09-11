{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.IModel where

import Data.Drasil.Quantities.Physics (time)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.DataDefs
import Drasil.PDController.GenDefs
import Drasil.PDController.References
import Drasil.PDController.TModel
import Language.Drasil
import Theory.Drasil (InstanceModel, im, qwC, newDEModel')
import Utils.Drasil (weave)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.PDController.Unitals

instanceModels :: [InstanceModel]
instanceModels = [imPD]

----------------------------------------------

imPD :: InstanceModel
imPD
  = im (newDEModel' imPDRC)
      [qwC qdSetPointTD $ UpFrom (Exc, exactDbl 0), qwC qdPropGain $ UpFrom (Exc, exactDbl 0),
       qwC qdDerivGain $ UpFrom (Exc, exactDbl 0)]
      (qw qdProcessVariableTD)
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
      where lhs = [exactDbl 1 $+ sy qdDerivGain $** (opProcessVariable $^^ 1)]
                  $++ (exactDbl 1 $** (opProcessVariable $^^ 2))
                  $++ (exactDbl 20 $+ sy qdPropGain $** (opProcessVariable $^^ 0))
            rhs = sy qdSetPointTD $* sy qdPropGain
      -- Matrix form: 
      -- coeffs = [[exactDbl 1, exactDbl 1 $+ sy qdDerivGain, exactDbl 20 $+ sy qdPropGain]]
      -- unknowns = [2, 1, 0]
      -- constants = [sy qdSetPointTD $* sy qdPropGain]

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
      [atStartNP (the processVariable), eS' qdProcessVariableFD, S "in a", phrase pidCL +:+
         S "is the product" `S.ofThe` phrase processError, fromSource ddErrSig `sC`
         phrase controlVariable, fromSource ddCtrlVar `sC` EmptyS
         `S.andThe` phrase powerPlant, fromSource gdPowerPlant]

derivEqn1 :: ModelExpr
derivEqn1
  = sy qdProcessVariableFD
      $= (sy qdSetPointFD $- sy qdProcessVariableFD)
      $* (sy qdPropGain $+ (sy qdDerivGain $* sy qdFreqDomain))
      $* recip_ (square (sy qdFreqDomain) $+ sy qdFreqDomain $+ exactDbl 20)

derivStmt2 :: Sentence
derivStmt2 = (S "Substituting the values and rearranging the equation" !.)

derivEqn2 :: ModelExpr
derivEqn2
  = square (sy qdFreqDomain) $* sy qdProcessVariableFD
      $+ ((exactDbl 1 $+ sy qdDerivGain) $* sy qdProcessVariableFD $* sy qdFreqDomain)
      $+ ((exactDbl 20 $+ sy qdPropGain) $* sy qdProcessVariableFD)
      $- (sy qdSetPointFD $* sy qdFreqDomain $* sy qdDerivGain)
      $- (sy qdSetPointFD $* sy qdPropGain) $= exactDbl 0

derivStmt3 :: Sentence
derivStmt3
  = S "Computing the" +:+ phrase qdInvLaplaceTransform +:+
     fromSource tmInvLaplace +:+. S "of the equation"

derivEqn3 :: ModelExpr
derivEqn3
  = deriv (deriv (sy qdProcessVariableTD) time) time $+
      (((exactDbl 1 $+ sy qdDerivGain) $* deriv (sy qdProcessVariableTD) time)
      $+ ((exactDbl 20 $+ sy qdPropGain) $* sy qdProcessVariableTD))
      $- (sy qdDerivGain $* deriv (sy qdSetPointTD) time)
      $- (sy qdSetPointTD $* sy qdPropGain) $= exactDbl 0

derivStmt4 :: Sentence
derivStmt4
  = foldlSent_
      [atStartNP (the setPoint), eS' qdSetPointTD, S "is a step function and a constant" +:+.
         fromSource aSP,
       S "Therefore the",
         S "differential" `S.ofThe` S "set point" `S.is` S "zero. Hence the equation",
         S "reduces to"]

derivEqn4 :: ModelExpr
derivEqn4
  = deriv (deriv (sy qdProcessVariableTD) time) time $+
      ((exactDbl 1 $+ sy qdDerivGain) $* deriv (sy qdProcessVariableTD) time)
      $+ ((exactDbl 20 $+ sy qdPropGain) $* sy qdProcessVariableTD)
      $- (sy qdSetPointTD $* sy qdPropGain) $= exactDbl 0
