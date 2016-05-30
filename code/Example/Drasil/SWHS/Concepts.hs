{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.Concepts where

import Example.Drasil.SWHS.Units

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

---Acronyms---
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,
  phsChgMtrl,physSyst,requirement,srs,progName,thModel]

assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,phsChgMtrl,
  physSyst,requirement,srs,progName,thModel :: ConceptChunk

assumption  = makeCC "A" "Assumption"
dataDefn    = makeCC "DD" "Data Definition"
genDefn     = makeCC "GD" "General Definition"
goalStmt    = makeCC "GS" "Goal Statement"
inModel     = makeCC "IM" "Instance Model"
likelyChg   = makeCC "LC" "Likely Change"
ordDiffEq   = makeCC "ODE" "Ordinary Differential Equation"
phsChgMtrl  = makeCC "PCM" "Phase Change Material"
physSyst    = makeCC "PS" "Physical System Description"
requirement = makeCC "R" "Requirement"
srs         = makeCC "SRS" "Software Requirements Specification"
progName    = makeCC "SWHS" "Solar Water Heating System"
thModel     = makeCC "T" "Theoretical Model"

---ConceptChunks---

heat_flux, phase_change_material, specific_heat, thermal_conduction,
  transient :: ConceptChunk

heat_flux = makeCC "Heat flux" "The rate of heat energy transfer per unit area."
phase_change_material = makeCC "Phase Change Material (PCM)" "A substance that uses phase changes (melting) to absorb or release large amounts of heat at a constant temperature"
specific_heat = makeCC "Specific heat" "Heat capacity per unit mass."
thermal_conduction = makeCC "Thermal conduction" "The transfer of heat energy through a substance."
transient = makeCC "Transient" "Changing with time."