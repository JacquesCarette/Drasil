module Drasil.GlassBR.Changes (likelyChgsList, unlikelyChgsList) where

--A list of likely and unlikely changes for GlassBR

import Language.Drasil
import Drasil.DocLang (mkLklyChnk, mkUnLklyChnk)

import Data.Drasil.Concepts.Documentation (condition, goal, input_, software, system, value, variable)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (flexure)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, foldlSent, ofThe')

import Drasil.GlassBR.Assumptions (glassCondition, explainScenario, standardValues, glassLite, 
  boundaryConditions, responseType, ldfConstant, assumptionConstants)
import Drasil.GlassBR.Concepts (blastRisk, glaSlab, glass)
import Drasil.GlassBR.Unitals (explosion, lite)

{--LIKELY CHANGES--}

likelyChgsList :: [LabelledContent]
likelyChgsList = [calcInternalBlastRisk, varValsOfmkE, accMoreThanSingleLite,
  accMoreBoundaryConditions, considerMoreThanFlexGlass]

calcInternalBlastRisk, varValsOfmkE, accMoreThanSingleLite, accMoreBoundaryConditions,
  considerMoreThanFlexGlass :: LabelledContent

calcInternalBlastRisk     = mkLklyChnk "calcInternalBlastRisk"     (calcInternalBlastRiskDesc (blastRisk)) "Calculate-Internal-Blask-Risk"
varValsOfmkE              = mkLklyChnk "varValsOfmkE"              (varValsOfmkEDesc)                      "Variable-Values-of-m,k,E"
accMoreThanSingleLite     = mkLklyChnk "accMoreThanSingleLite"     (accMoreThanSingleLiteDesc)             "Accomodate-More-than-Single-Lite"
accMoreBoundaryConditions = mkLklyChnk "accMoreBoundaryConditions" (accMoreBoundaryConditionsDesc)         "Accomodate-More-Boundary-Conditions"
considerMoreThanFlexGlass = mkLklyChnk "considerMoreThanFlexGlass" (considerMoreThanFlexGlassDesc)         "Consider-More-than-Flexure-Glass"

calcInternalBlastRiskDesc :: NamedChunk -> Sentence
varValsOfmkEDesc, accMoreThanSingleLiteDesc, accMoreBoundaryConditionsDesc, considerMoreThanFlexGlassDesc :: Sentence

calcInternalBlastRiskDesc mainConcept = foldlSent [(makeRef explainScenario) `sDash` S "The",
  phrase system, S "currently only calculates for external" +:+.
  phrase mainConcept, S "In the future,", plural calculation,
  S "can be added for the internal", phrase mainConcept]

varValsOfmkEDesc = foldlSent [(makeRef standardValues) `sC` ((makeRef ldfConstant) `sDash`
  S "Currently, the"), plural value, S "for",
  foldlList Comma List (map ch (take 3 assumptionConstants)),
  S "are assumed to be the same for all" +:+. phrase glass,
  S "In the future, these", plural value, S "can be changed to",
  phrase variable, plural input_]

accMoreThanSingleLiteDesc = foldlSent [(makeRef glassLite) `sDash` S "The", phrase software,
  S "may be changed to accommodate more than a single", phrase lite]

accMoreBoundaryConditionsDesc = foldlSent [(makeRef boundaryConditions) `sDash` S "The", phrase software,
  S "may be changed to accommodate more boundary", plural condition,
  S "than 4-sided support"]

considerMoreThanFlexGlassDesc = foldlSent [(makeRef responseType) `sDash` S "The", phrase software,
  S "may be changed to consider more than just", phrase flexure,
  S "of the glass"]

{--UNLIKELY CHANGES--}

unlikelyChgsList :: [LabelledContent]
unlikelyChgsList = [predictWithstandOfCertDeg, accAlteredGlass]

predictWithstandOfCertDeg, accAlteredGlass :: LabelledContent

predictWithstandOfCertDeg = mkUnLklyChnk "predictWithstandOfCertDeg" predictWithstandOfCertDegDesc "Predict-Withstanding-of-Certain-Degree"
accAlteredGlass           = mkUnLklyChnk "accAlteredGlass"           accAlteredGlassDesc           "Accommodate-Altered-Glass"

predictWithstandOfCertDegDesc, accAlteredGlassDesc :: Sentence

predictWithstandOfCertDegDesc = foldlSent [phrase goal `ofThe'` phrase system,
  S "is to predict whether the", phrase glaSlab, S "under consideration can",
  S "withstand an", phrase explosion, S "of a certain degree"]

accAlteredGlassDesc = foldlSent [makeRef glassCondition, S "requires that the", phrase glass +:+.
  S "is not altered in any way", S "Therefore, this cannot be used on altered",
  phrase glass]
