module Drasil.SSP.Requirements (sspRequirements, sspInputDataTable) where

import Language.Drasil

import Data.Drasil.Concepts.Computation (inDatum)
import Data.Drasil.Concepts.Documentation (constraint, datum, funcReqDom, 
  input_, method_, physicalConstraint, value)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), 
  foldlList, foldlSent, ofThe, sAnd, sOf)
import Data.Drasil.Utils (mkInputDatTb)

import Drasil.SSP.DataCons (data_constraint_Table2, data_constraint_Table3)
import Drasil.SSP.Defs (crtSlpSrf, morPrice, slice, slope, slpSrf)
import Drasil.SSP.IMods (fctSfty, nrmShrFor, intsliceFs, crtSlpId)
import Drasil.SSP.Unitals (coords, fs, fs_min, sspInputs)

sspRequirements :: [ConceptInstance]
sspRequirements = [readAndStore, verifyInput, generateCSS, calculateFS, 
  determineCritSlip, verifyOutput, displayGraph]

readAndStore, verifyInput, generateCSS, calculateFS, determineCritSlip, 
  verifyOutput, displayGraph :: ConceptInstance

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

displayGraph = cic "displayGraph" ( foldlSent [
  S "Display the", phrase crtSlpSrf, S "graphically. Display the", phrase value 
  `ofThe` phrase fs]) 
 "Display-Graph" funcReqDom

------------------
sspInputDataTable :: LabelledContent
sspInputDataTable = mkInputDatTb $ dqdWr coords : map dqdWr sspInputs
  --FIXME: this has to be seperate since coords is a different type
