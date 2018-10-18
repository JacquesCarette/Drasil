module Drasil.GlassBR.Changes (likelyChgs, likelyChgsList, unlikelyChgs,
  unlikelyChgsList) where

--A list of likely and unlikely changes for GlassBR

import Language.Drasil

import Drasil.DocLang (mkEnumSimpleD)

import Data.Drasil.Concepts.Documentation (condition, goal, input_, likeChgDom,
  software, system, unlikeChgDom, value, variable)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (flexure)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, foldlSent, ofThe')

import Drasil.GlassBR.Assumptions (glassCondition, explainScenario, standardValues, glassLite, 
  boundaryConditions, responseType, ldfConstant, assumptionConstants)
import Drasil.GlassBR.Concepts (blastRisk, glaSlab, glass)
import Drasil.GlassBR.Unitals (explosion, lite)

{--LIKELY CHANGES--}

likelyChgsList :: [Contents]
likelyChgsList = mkEnumSimpleD likelyChgs

likelyChgs :: [ConceptInstance]
likelyChgs = [calcInternalBlastRisk, varValsOfmkE, accMoreThanSingleLite,
  accMoreBoundaryConditions, considerMoreThanFlexGlass]

calcInternalBlastRisk, varValsOfmkE, accMoreThanSingleLite, accMoreBoundaryConditions,
  considerMoreThanFlexGlass :: ConceptInstance

calcInternalBlastRisk     = cic "calcInternalBlastRisk"     (calcInternalBlastRiskDesc blastRisk) "Calculate-Internal-Blask-Risk"       likeChgDom
varValsOfmkE              = cic "varValsOfmkE"              varValsOfmkEDesc                      "Variable-Values-of-m,k,E"            likeChgDom
accMoreThanSingleLite     = cic "accMoreThanSingleLite"     accMoreThanSingleLiteDesc             "Accomodate-More-than-Single-Lite"    likeChgDom
accMoreBoundaryConditions = cic "accMoreBoundaryConditions" accMoreBoundaryConditionsDesc         "Accomodate-More-Boundary-Conditions" likeChgDom
considerMoreThanFlexGlass = cic "considerMoreThanFlexGlass" considerMoreThanFlexGlassDesc         "Consider-More-than-Flexure-Glass"    likeChgDom

calcInternalBlastRiskDesc :: NamedChunk -> Sentence
varValsOfmkEDesc, accMoreThanSingleLiteDesc, accMoreBoundaryConditionsDesc, considerMoreThanFlexGlassDesc :: Sentence

calcInternalBlastRiskDesc mainConcept = foldlSent [(makeRef explainScenario) +:+ S "- The",
  phrase system, S "currently only calculates for external" +:+.
  phrase mainConcept, S "In the future,", plural calculation,
  S "can be added for the internal", phrase mainConcept]

varValsOfmkEDesc = foldlSent [(makeRef standardValues) `sC` ((makeRef ldfConstant) +:+
  S "- Currently, the"), plural value, S "for",
  foldlList Comma List (map ch (take 3 assumptionConstants)),
  S "are assumed to be the same for all" +:+. phrase glass,
  S "In the future, these", plural value, S "can be changed to",
  phrase variable, plural input_]

accMoreThanSingleLiteDesc = foldlSent [(makeRef glassLite) +:+ S "- The", phrase software,
  S "may be changed to accommodate more than a single", phrase lite]

accMoreBoundaryConditionsDesc = foldlSent [(makeRef boundaryConditions) :+: S "- The", phrase software,
  S "may be changed to accommodate more boundary", plural condition,
  S "than 4-sided support"]

considerMoreThanFlexGlassDesc = foldlSent [(makeRef responseType) +:+ S "- The", phrase software,
  S "may be changed to consider more than just", phrase flexure,
  S "of the glass"]

{--UNLIKELY CHANGES--}

unlikelyChgsList :: [Contents]
unlikelyChgsList = mkEnumSimpleD unlikelyChgs

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [predictWithstandOfCertDeg, accAlteredGlass]

predictWithstandOfCertDeg, accAlteredGlass :: ConceptInstance

predictWithstandOfCertDeg = cic "predictWithstandOfCertDeg" predictWithstandOfCertDegDesc "Predict-Withstanding-of-Certain-Degree"  unlikeChgDom
accAlteredGlass           = cic "accAlteredGlass"           accAlteredGlassDesc           "Accommodate-Altered-Glass"               unlikeChgDom

predictWithstandOfCertDegDesc, accAlteredGlassDesc :: Sentence

predictWithstandOfCertDegDesc = foldlSent [phrase goal `ofThe'` phrase system,
  S "is to predict whether the", phrase glaSlab, S "under consideration can",
  S "withstand an", phrase explosion, S "of a certain degree"]

accAlteredGlassDesc = foldlSent [makeRef glassCondition, S "requires that the", phrase glass +:+.
  S "is not altered in any way", S "Therefore, this cannot be used on altered",
  phrase glass]
