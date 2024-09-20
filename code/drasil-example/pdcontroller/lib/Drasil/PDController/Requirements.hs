{-#LANGUAGE PostfixOperators#-}
module Drasil.PDController.Requirements where

import Data.Drasil.Concepts.Documentation (funcReqDom, datumConstraint)
import Drasil.DocLang.SRS (datCon)
import Drasil.DocLang (mkMaintainableNFR, mkPortableNFR, mkVerifiableNFR, mkSecurityNFR)

import Drasil.PDController.Concepts
import Drasil.PDController.IModel

import Language.Drasil

funcReqs :: [ConceptInstance]
funcReqs = [verifyInputs, calculateValues, outputValues]

verifyInputs, calculateValues, outputValues :: ConceptInstance
verifyInputs
  = cic "verifyInputs" verifyInputsDesc "Verify-Input-Values" funcReqDom
calculateValues
  = cic "calculateValues" calculateValuesDesc "Calculate-Values" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

verifyInputsDesc, calculateValuesDesc, outputValuesDesc :: Sentence

verifyInputsDesc
  = foldlSent_
      [S "Ensure that the input values are within the",
         S "limits specified in the"
         +:+. namedRef (datCon [] []) (plural datumConstraint)]

calculateValuesDesc
  = foldlSent
      [S "Calculate the", phrase processVariable, fromSource imPD,
         S "over the simulation time"]

outputValuesDesc
  = foldlSent
      [S "Output the", phrase processVariable, fromSource imPD,
         S "over the simulation time"]

-----------------------------------------------------------------------------

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [portability, security, maintainability, verifiability]

portability :: ConceptInstance
portability = mkPortableNFR "portable" ["Windows", "Mac OSX", "Linux"] "Portability"

security :: ConceptInstance
security = mkSecurityNFR "security" "Security"

maintainability :: ConceptInstance
maintainability = mkMaintainableNFR "maintainability" 10 "Maintainability"

verifiability :: ConceptInstance
verifiability = mkVerifiableNFR "verifiability" "Verifiability"
