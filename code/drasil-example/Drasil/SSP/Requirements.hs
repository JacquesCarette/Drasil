module Drasil.SSP.Requirements (sspRequirements, sspInputDataTable) where

import Language.Drasil

import Data.Drasil.Concepts.Computation (inDatum)
import Data.Drasil.Concepts.Documentation (constraint, datum, funcReqDom, 
  input_, method_, physical, value)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), 
  foldlList, foldlSent, ofThe, sOf)
import Data.Drasil.Utils (mkInputDatTb)

import Drasil.SSP.DataCons (data_constraint_Table2)
import Drasil.SSP.Defs (crtSlpSrf, morPrice, slice, slope, slpSrf)
import Drasil.SSP.IMods (crtSlpId)
import Drasil.SSP.Unitals (coords, fs, fs_min, sspInputs)

sspRequirements :: [ConceptInstance]
sspRequirements = [readAndStore, verifyInput, generateCSS, testSlipSrf, prepareSlipS, 
    calculateFS, rankSlope, generateCSS', repeatFindFS, prepareCSS, 
    calculateFS', displayGraph]

readAndStore, verifyInput, generateCSS, testSlipSrf, prepareSlipS, calculateFS, rankSlope, 
    generateCSS', repeatFindFS, prepareCSS, calculateFS', 
    displayGraph :: ConceptInstance

readAndStore = cic "readAndStore" ( foldlSent [
  S "Read the", plural input_ `sC` S "shown in", 
  makeRef2S sspInputDataTable `sC` S "and store the", plural datum]) "Read-and-Store" funcReqDom

verifyInput = cic "verifyInput" ( foldlSent [
  S "Verify that the", phrase input_, plural datum, S "lie within the",
  phrase physical, plural constraint, S "shown in", 
  makeRef2S data_constraint_Table2]) "Verify-Input" funcReqDom

generateCSS = cic "generateCSS" ( foldlSent [
  S "Generate potential", plural crtSlpSrf, S "for the", 
  phrase input_, phrase slope, sParen (S "using" +:+ makeRef2S crtSlpId)]) "Generate-Critical-Slip-Surfaces" funcReqDom

testSlipSrf = cic "testSlipSrf" ( foldlSent [
  S "Test the", plural slpSrf, S "to determine if they are physically",
  S "realizable based on a set of pass or fail criteria"]) "Test-Slip-Surfaces"
  funcReqDom

prepareSlipS = cic "prepareSlipS" ( foldlSent [
  S "Prepare the", plural slpSrf, S "for a", phrase method_ `sOf`
  plural slice, S "or limit equilibrium analysis"]) "Prepare-Slip-Surfaces"
  funcReqDom

calculateFS = cic "calculateFS" ( foldlSent [
  S "Calculate", plural fs `ofThe` plural slpSrf]) "Calculate-Factors-of-Safety"
  funcReqDom

rankSlope = cic "rankSlope" ( foldlSent [
  S "Rank and weight the", plural slope, S "based on their", 
  phrase fs `sC` S "such that a", phrase slpSrf, S "with a smaller", 
  phrase fs, S "has a larger weighting"]) "Rank-and-Weight-Slopes" funcReqDom

generateCSS' = cic "generateCSS'" ( foldlSent [
  S "Generate new potential", plural crtSlpSrf, 
  S "based on previously analysed", plural slpSrf, S "with low", 
  plural fs]) "Generate-New-Critical-Slip-Surfaces" funcReqDom

repeatFindFS = cic "repeatFindFS" ( foldlSent [
  S "Repeat", (foldlList Comma List $ map makeRef2S [testSlipSrf, prepareSlipS,
  calculateFS, rankSlope, generateCSS']), S "until the", phrase fs_min,
  S "remains approximately the same over a",
  S "predetermined number of repetitions. Identify the", phrase slpSrf, 
  S "that generates the", phrase fs_min, S "as the", phrase crtSlpSrf])
  "Repeat-Find-Factor-of-Safety" funcReqDom

prepareCSS = cic "prepareCSS" ( foldlSent [
  S "Prepare the", phrase crtSlpSrf, S "for", phrase method_ `sOf` 
  plural slice, S "or limit equilibrium analysis"])
  "Prepare-Critical-Slip-Surface" funcReqDom

calculateFS' = cic "calculateFS'" ( foldlSent [
  S "Calculate", phrase fs `ofThe` phrase crtSlpSrf, 
  S "using the", titleize morPrice, phrase method_])
  "Calculate-Final-Factor-of-Safety" funcReqDom

displayGraph = cic "displayGraph" ( foldlSent [
  S "Display the", phrase crtSlpSrf, S "graphically. Display the", phrase value 
  `ofThe` phrase fs]) 
 "Display-Graph" funcReqDom

------------------
sspInputDataTable :: LabelledContent
sspInputDataTable = mkInputDatTb $ dqdWr coords : map dqdWr sspInputs
  --FIXME: this has to be seperate since coords is a different type
