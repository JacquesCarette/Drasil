module Drasil.PDController.Requirements (
  funcReqs, nonfuncReqs, funcReqsTables
) where

import qualified Data.List.NonEmpty as NE (NonEmpty, fromList)

import Data.Drasil.Concepts.Documentation (funcReqDom, datumConstraint)
import Drasil.SRS.Concepts (datCon)
import Drasil.SRS (mkMaintainableNFR, mkPortableNFR, mkVerifiableNFR,
  mkSecurityNFR, inReqWTab, mkQRTuple, outReq)

import Drasil.PDController.Concepts
import Drasil.PDController.IModel
import Drasil.PDController.Unitals (inputs)

import Language.Drasil
import Language.Drasil.Document

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInputs, calculateValues, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

inputValues, outputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs
(outputValues, _) = outReq (Just $ S "over the simulation time") outputsWReqs

outputsWReqs :: NE.NonEmpty (DefinedQuantityDict, Sentence)
outputsWReqs = NE.fromList $ mkQRTuple instanceModels

verifyInputs, calculateValues :: ConceptInstance
verifyInputs
  = cic "verifyInputs" verifyInputsDesc "Verify-Input-Values" funcReqDom
calculateValues
  = cic "calculateValues" calculateValuesDesc "Calculate-Values" funcReqDom

verifyInputsDesc, calculateValuesDesc :: Sentence

verifyInputsDesc
  = foldlSent_
      [S "Ensure that the input values are within the",
         S "limits specified in the"
         +:+. namedRef (datCon [] []) (plural datumConstraint)]

calculateValuesDesc
  = foldlSent
      [S "Calculate the", phrase processVariable, fromSource imPD,
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
