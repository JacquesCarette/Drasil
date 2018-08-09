module Drasil.SSP.Requirements (sspRequirements, sspInputDataTable) where

import Language.Drasil
import Drasil.DocLang (mkRequirement)

import Data.Drasil.Concepts.Computation (inDatum)
import Data.Drasil.Concepts.Documentation (datum, element, input_, method_, 
  requirement, value)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), andThe, 
  foldlList, foldlSent, ofThe, sOf)
import Data.Drasil.Utils (mkInputDatTb)

import Drasil.SSP.Defs (crtSlpSrf, morPrice, slice, slope, slpSrf)
import Drasil.SSP.Unitals (coords, fs, fs_min, sspInputs)

sspRequirements :: [LabelledContent]
sspRequirements = [req1, req2, req3, req4, req5, req6, req7, req8, req9, req10, req11]

req1, req2, req3, req4, req5, req6, req7, req8, req9, req10, req11 :: LabelledContent

req1 = mkRequirement "req1" ( foldlSent [
  S "Read the", phrase input_, S "file and store the" +:+. 
  plural datum, S "Necessary", plural inDatum, S "summarized in", 
  makeRef sspInputDataTable]) "Read-and-Store"

req2 = mkRequirement "req2" ( foldlSent [
  S "Generate potential", plural crtSlpSrf,S "for the", 
  phrase input_, phrase slope]) "Generate-Critical-Slip-Surfaces"

req3 = mkRequirement "req3" ( foldlSent [
  S "Test the", plural slpSrf, S "to determine if they are physically",
  S "realizable based on a set of pass or fail criteria"]) "Test-Slip-Surfaces"

req4 = mkRequirement "req4" ( foldlSent [
  S "Prepare the", plural slpSrf, S "for a", phrase method_ `sOf`
  plural slice, S "or limit equilibrium analysis"]) "Prepare-Slip-Surfaces"

req5 = mkRequirement "req5" ( foldlSent [
  S "Calculate", plural fs `ofThe` plural slpSrf]) "Calculate-Factors-of-Safety"

req6 = mkRequirement "req6" ( foldlSent [
  S "Rank and weight the", plural slope, S "based on their", 
  phrase fs `sC` S "such that a", phrase slpSrf, S "with a smaller", 
  phrase fs, S "has a larger weighting"]) "Rank-and-Weight-Slopes"

req7 = mkRequirement "req7" ( foldlSent [
  S "Generate new potential", plural crtSlpSrf, 
  S "based on previously analysed", plural slpSrf, S "with low", 
  plural fs]) "Generate-New-Critical-Slip-Surfaces"

req8 = mkRequirement "req8" ( foldlSent [
  S "Repeat", (foldlList Comma List $ map makeRef [req3, req4, req5, req6, req7]), 
  S "until the", phrase fs_min, S "remains approximately the same over a",
  S "predetermined number of repetitions. Identify the", phrase slpSrf, 
  S "that generates the", phrase fs_min, S "as the", phrase crtSlpSrf])
  "Repeat-Find-Factor-of-Safety"

req9 = mkRequirement "req9" ( foldlSent [
  S "Prepare the", phrase crtSlpSrf, S "for", phrase method_ `sOf` 
  plural slice, S "or limit equilibrium analysis"])
  "Prepare-Critical-Slip-Surface"

req10 = mkRequirement "req10" ( foldlSent [
  S "Calculate", phrase fs `ofThe` phrase crtSlpSrf, 
  S "using the", titleize morPrice, phrase method_])
  "Calculate-Final-Factor-of-Safety"

req11 = mkRequirement "req11" ( foldlSent [
  S "Display the", phrase crtSlpSrf `andThe` phrase slice, 
  phrase element, S "displacements graphically. Give", plural value `ofThe` 
  plural fs, S "calculated by the", titleize morPrice, phrase method_]) 
  "Display-Graph"

------------------
sspInputDataTable :: LabelledContent
sspInputDataTable = mkInputDatTb ([dqdWr coords] ++ map dqdWr sspInputs)
  --FIXME: this has to be seperate since coords is a different type