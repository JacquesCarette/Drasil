module Drasil.SSP.IMods where

import Language.Drasil
import Drasil.SSP.Units
import Drasil.SSP.Defs
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Control.Lens ((^.))


-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [RelationConcept]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, forDisEqlb, rfemFoS, crtSlpId]

--
fctSfty :: RelationConcept
fctSfty = makeRC "fctSfty" factorOfSafety fcSfty_desc fcSfty_rel

fcSfty_rel :: Relation
fcSfty_rel = (C fs) := (Int 0) --FIXME: add the long equation

fcSfty_desc :: Sentence
fcSfty_desc = foldlSent [S "Equation for the Factor of Safety" `isThe` S "ratio between resistive",
  S "and mobile shear of the slip surface. The sum of values from each slice is taken to find",
  S "the total resistive and mobile shear for the slip surface. The constants", P (Greek Phi),
  S "and", P (Greek Psi), S "convert the resistive and mobile shear without the inluence of", 
  S "interslice forces, to a calculation considering the interslice forces"]

--
nrmShrFor :: RelationConcept
nrmShrFor = makeRC "nrmShrFor" (nounPhraseSP "normal/shear force ratio") nrmShrF_desc nrmShrF_rel

nrmShrF_rel :: Relation
nrmShrF_rel = (C fs) := (Int 0) --FIXME: add the long equation

nrmShrF_desc :: Sentence
nrmShrF_desc = foldlSent [P (Greek Lambda_L) `isThe` S "magnitude ratio between shear and", 
  S "normal forces at the interslice interfaces as the assumption of the Morgenstern Price", 
  S "method in GD5. The inclination function f determines the relative magnitude ratio",
  S "between the different interslices, while", P (Greek Lambda_L), S "determines the" +:+. 
  S "magnitude", P (Greek Lambda_L), S "uses the sum of interslice normal and shear forces",
  S "taken from each interslice"] --FIXME: does "i" need to be pulled out?

--
intsliceFs :: RelationConcept
intsliceFs = makeRC "intsliceFs" (nounPhraseSP "interslice forces") sliceFs_desc sliceFs_rel

sliceFs_rel :: Relation
sliceFs_rel = (C intNormForce) := (Int 0) --(C intNormForce) := Case [
  --(((C fs) * (C ti) :- (C ri)) :/ (Int 100), (Int 1)),
  --((((Int 100) * (C intNormForce) :+ (C fs) * (C ti) :- (C ri)):/Int 100), Int 1),
  --((Int 0), (Int 0))]
  --FIXME: update the long equation; where is Phi and Psi in .Units?

sliceFs_desc :: Sentence
sliceFs_desc = foldlSent [S "The value of the interslice normal force", (P $ intNormForce ^. symbol),
  S "at interface i. The net force" `isThe` S "weight of the slices adjacent to interface i exert", 
  S "horizontally on each other"] --FIXME: does "i" need to be pulled out?

--
forDisEqlb :: RelationConcept
forDisEqlb = makeRC "forDisEqlb" (nounPhraseSP "force displacement equilibrium") fDisEq_desc fDisEq_rel

fDisEq_rel :: Relation
fDisEq_rel = ((C qi)*(Language.Drasil.sin(C omega_i)):-(C dHi):-
  (C kc)*(C slcWght):-(C baseHydroForce)*(Language.Drasil.sin(C baseAngle)):+
  (C surfHydroForce)*(Language.Drasil.sin(C beta_i))) := (Int 1) 
  --FIXME: add the other long equation (i.e. Y Equilibrium)
  --FIXME: add the second part of X Equilibrium equation

fDisEq_desc :: Sentence
fDisEq_desc = foldlSent [S "There is one set of force displacement equilibrium",
  S "equations in the x and y directions for each element. System of equations",
  S "solved for displacements (", (P $ dx_i ^. symbol), S "and",
  (P $ dy_i ^. symbol), S ")", (P $ dHi ^. symbol), S "=", (P $ hi ^. symbol)
  `isThe` S "net hydrostatic force across a slice.", (P $ kc ^. symbol)
  `isThe` S "earthquake load factor.", (P $ slcWght ^. symbol)
  `isThe` S "weight of the slice.", (P $ baseHydroForce ^. symbol) 
  `isThe` S "pore water pressure acting on the slice base.", (P $ surfHydroForce ^. symbol)
  `isThe` S "pore water pressure acting on the slice surface.", (P $ baseAngle ^. symbol)
  `isThe` S "angle of the base with the horizontal.", (P $ beta_i ^. symbol)
  `isThe` S "angle of the surface with the horizontal.", (P $ dx_i ^. symbol)
  `isThe` S "x displacement of slice i.", (P $ dy_i ^. symbol)
  `isThe` S "y displacement of slice i.", (P $ surfLngth ^. symbol) 
  `isThe` S "length of the interslice surface i.", (P $ baseLngth ^. symbol)
  `isThe` S "length of the base surface i.", (P $ k_sti ^. symbol)
  `isThe` S "interslice shear stiffness at surface i.", S " Kst,i-1"
  `isThe` S "interslice normal stiffness at surface i. KbA,i, and KbB,i",
  S "are the base stiffness values for slice i"]

--
rfemFoS :: RelationConcept
rfemFoS = makeRC "rfemFoS" (nounPhraseSP "RFEM factor of safety") rfemFoS_desc rfemFoS_rel

rfemFoS_rel :: Relation
rfemFoS_rel = (C fsloc) := ((C cohesion) :- (C k_bni)*(C dv_i)*(Language.Drasil.tan(C fricAngle))):/((C k_bti)*(C du_i)) 
  --FIXME: add the other long equation

rfemFoS_desc :: Sentence
rfemFoS_desc = foldlSent [(P $ fsloc ^. symbol) `isThe` S "factor of safety for slice i.",
  (P $ fs ^. symbol) `isThe` S "factor of safety for the entire slip surface.",
  (P $ cohesion ^. symbol) `isThe` S "cohesion of slice i's base.",
  (P $ fricAngle ^. symbol) `isThe` (phrase fricAngle), S "of slice i's base.",
  (P $ dv_i ^. symbol) `isThe` S "normal displacement of slice i.",
  (P $ du_i ^. symbol) `isThe` S "shear displacement of slice i.",
  (P $ k_bti ^. symbol) `isThe` S "length of the base of slice i.",
  (P $ k_bni ^. symbol) `isThe` S "base normal stiffness at surface i.",
  (P $ n ^. symbol) `isThe` S "number of slices in the slip surface"]

--
crtSlpId :: RelationConcept
crtSlpId = makeRC "crtSlpId" (nounPhraseSP "critical slip identification") crtSlpId_desc crtSlpId_rel

crtSlpId_rel :: Relation
crtSlpId_rel = (C fs) := (C minFunction) :*  (C critCoords)
--FIXME: use brackets and comma for this equation rather than :*

crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent [S "Given the necessary slope inputs, a minimization algorithm",
  S "or function", (P $ minFunction ^. symbol), S "will identify the critical slip surface", 
  S "of the slope, with the critical slip coordinates", (P $ critCoords ^. symbol), 
  S "and the minimum factor of safety FSmin that results"]