{-#LANGUAGE PostfixOperators#-}
module Drasil.PDController.Requirements where

import Data.Drasil.Concepts.Computation (os)
import Data.Drasil.Concepts.Documentation (datumConstraint, funcReqDom,
  nonFuncReqDom, vavPlan)

import Drasil.DocLang.SRS (datCon)

import Drasil.PDController.Concepts
import Drasil.PDController.IModel

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

funcReqs :: [ConceptInstance]
funcReqs = [verifyInputs, calculateValues]

verifyInputs, calculateValues :: ConceptInstance
verifyInputs
  = cic "verifyInputs" verifyInputsDesc "Verify-Input-Values" funcReqDom
calculateValues
  = cic "calculateValues" calculateValuesDesc "Calculate-Values" funcReqDom

verifyInputsDesc, calculateValuesDesc :: Sentence

verifyInputsDesc
  = (S "Ensure that the input values are within the limits specified" `S.inThe`
      namedRef (datCon [] []) (plural datumConstraint) !.)

calculateValuesDesc
  = foldlSent
      [S "Calculate the", phrase processVariable, fromSource imPD,
         S "over the simulation time"]

-----------------------------------------------------------------------------

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [portability, security, maintainability, verifiability]

portability :: ConceptInstance
portability
  = cic "portability"
      (S "The code shall be portable to multiple" +:+. plural os)
      "Portable"
      nonFuncReqDom

security :: ConceptInstance
security
  = cic "security"
      (foldlSent
         [S "The code shall be immune to common security problems such as memory",
            S "leaks, divide by zero errors, and the square root of negative numbers"])
      "Secure"
      nonFuncReqDom

maintainability :: ConceptInstance
maintainability
  = cic "maintainability"
      (foldlSent
         [S "The dependencies among the instance models, requirements,",
            S "likely changes, assumptions and all other relevant sections of",
            S "this document shall be traceable to each other in the trace matrix"])
      "Maintainable"
      nonFuncReqDom

verifiability :: ConceptInstance
verifiability
  = cic "verifiability"
      (S "The code shall be verifiable against a" +:+. phrase vavPlan)
      "Verifiable"
      nonFuncReqDom
