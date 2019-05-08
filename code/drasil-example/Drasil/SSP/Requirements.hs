module Drasil.SSP.Requirements (sspFRequirements, sspNFRequirements,
  sspInputDataTable, sspInputsToOutputTable, propsDeriv) where

import Language.Drasil

import Drasil.DocLang.SRS (propCorSol) 

import Data.Drasil.Concepts.Computation (inDatum)
import Data.Drasil.Concepts.Documentation (assumption, code, dataDefn,
  datum, funcReqDom, genDefn, inModel, input_, likelyChg, mg, mis, module_,
  name_, nonFuncReqDom, output_, physicalConstraint, property, requirement,
  srs, symbol_, thModel, traceyMatrix, unlikelyChg, user, value)
import Data.Drasil.Concepts.Physics (twoD)

import Data.Drasil.SentenceStructures (SepType(Comma), FoldType(List), 
  foldlList, foldlSent, foldlSP, ofThe, ofThe', sAnd)
import Data.Drasil.Utils (mkInputDatTb)

import Drasil.SSP.DataCons (data_constraint_Table2, data_constraint_Table3)
import Drasil.SSP.Defs (crtSlpSrf, slope)
import Drasil.SSP.IMods (fctSfty, nrmShrFor, intsliceFs, crtSlpId)
import Drasil.SSP.Unitals (constF, coords, fs, fs_min, intNormForce, 
  intShrForce, sspInputs, xMaxExtSlip, xMaxEtrSlip, xMinExtSlip, xMinEtrSlip, 
  yMaxSlip, yMinSlip)

{-Functional Requirements-}

sspFRequirements :: [ConceptInstance]
sspFRequirements = [readAndStore, verifyInput, generateCSS, calculateFS, 
  determineCritSlip, verifyOutput, displayInput, displayGraph, displayFS,
  displayNormal, displayShear, writeToFile]

readAndStore, verifyInput, generateCSS, calculateFS, determineCritSlip, 
  verifyOutput, displayInput, displayGraph, displayFS,
  displayNormal, displayShear, writeToFile :: ConceptInstance

readAndStore = cic "readAndStore" ( foldlSent [
  S "Read the", plural input_ `sC` S "shown in", 
  makeRef2S sspInputDataTable `sC` S "and store the", plural datum]) 
  "Read-and-Store" funcReqDom

verifyInput = cic "verifyInput" ( foldlSent [
  S "Verify that the", plural inDatum, S "lie within the",
  plural physicalConstraint, S "shown in", makeRef2S data_constraint_Table2])
  "Verify-Input" funcReqDom

generateCSS = cic "generateCSS" ( foldlSent [
  S "Generate potential", plural crtSlpSrf, S "for the", 
  phrase input_, phrase slope, sParen (S "using" +:+ makeRef2S crtSlpId)]) "Generate-Critical-Slip-Surfaces" funcReqDom

calculateFS = cic "calculateFS" ( foldlSent [
  S "Calculate the", plural fs, S "for each of the potential", 
  plural crtSlpSrf, sParen (S "using" +:+ makeRef2S fctSfty `sC` 
  makeRef2S nrmShrFor `sC` makeRef2S intsliceFs)])
  "Calculate-Factors-of-Safety" funcReqDom

determineCritSlip = cic "determineCritSlip" ( foldlSent [
  S "Compare the", phrase fs, S "for each potential", phrase crtSlpSrf,
  S "to determine the minimum", phrase fs `sC` S "corresponding to the", 
  phrase crtSlpSrf, sParen (S "using" +:+ makeRef2S crtSlpId)]) 
  "Determine-Critical-Slip-Surface" funcReqDom

verifyOutput = cic "verifyOutput" ( foldlSent [
  S "Verify that the", phrase fs_min `sAnd` phrase crtSlpSrf, S "satisfy the",
  plural physicalConstraint, S "shown in", makeRef2S data_constraint_Table3])
  "Verify-Output" funcReqDom

displayInput = cic "displayInput" ( foldlSent [
  S "Display as", phrase output_, S "the", phrase user :+: S "-supplied",
  plural input_, S "listed in", makeRef2S sspInputsToOutputTable])
  "Display-Input" funcReqDom

displayGraph = cic "displayGraph" ( foldlSent [
  S "Display", phrase crtSlpSrf `ofThe` short twoD, phrase slope `sC` 
  S "as determined from", makeRef2S crtSlpId `sC` S "graphically"]) 
  "Display-Graph" funcReqDom

displayFS = cic "displayFS" ( foldlSent [
  S "Display", phrase value `ofThe` phrase fs, S "for the", 
  phrase crtSlpSrf `sC` S "as determined from", makeRef2S fctSfty `sC`
  makeRef2S nrmShrFor `sC` S "and", makeRef2S intsliceFs]) 
  "Display-Factor-of-Safety" funcReqDom

displayNormal = cic "displayNormal" ( foldlSent [
  S "Using", makeRef2S fctSfty `sC` makeRef2S nrmShrFor `sC` S "and",
  makeRef2S intsliceFs `sC` S "calculate and graphically display the",
  plural intNormForce]) "Display-Interslice-Normal-Forces" funcReqDom

displayShear = cic "displayShear" ( foldlSent [
  S "Using", makeRef2S fctSfty `sC` makeRef2S nrmShrFor `sC` S "and",
  makeRef2S intsliceFs `sC` S "calculate and graphically display the",
  plural intShrForce]) "Display-Interslice-Shear-Forces" funcReqDom

writeToFile = cic "writeToFile" ( foldlSent [
  S "Provide the option of writing the output result data, as given in", 
  foldlList Comma List (map makeRef2S [displayInput, displayGraph, displayFS, 
  displayNormal, displayShear]) `sC` S "to a file"]) "Write-Results-To-File" 
  funcReqDom

------------------
sspInputDataTable :: LabelledContent
sspInputDataTable = mkInputDatTb $ dqdWr coords : map dqdWr sspInputs
  --FIXME: this has to be seperate since coords is a different type

inputsToOutput :: [DefinedQuantityDict]
inputsToOutput = constF : (map dqdWr [xMaxExtSlip, xMaxEtrSlip, xMinExtSlip, 
  xMinEtrSlip, yMaxSlip, yMinSlip])

sspInputsToOutputTable :: LabelledContent
sspInputsToOutputTable = llcc (makeTabRef "inputsToOutputTable") $
  Table [titleize symbol_, titleize name_] (mkTable [ch, phrase] inputsToOutput)
  (at_start' input_ +:+ S "to be returned as" +:+ phrase output_) True

{-Nonfunctional Requirements-}
sspNFRequirements :: [ConceptInstance]
sspNFRequirements = [correct, understandable, reusable, maintainable]

propsDeriv :: [Contents]
propsDeriv = [foldlSP [S "FIXME"]]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  plural output_ `ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol propsDeriv [])
  ]) "Correct" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "is modularized with complete",
  phrase mg `sAnd` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The", phrase code, S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix, S "in the", getAcc srs `sAnd` phrase mg]) "Maintainable" nonFuncReqDom
