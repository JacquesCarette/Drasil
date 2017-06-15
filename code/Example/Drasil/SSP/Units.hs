module Drasil.SSP.Units where

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Quantities.SolidMechanics as SM
import Data.Drasil.Concepts.Physics as CP
import Data.Drasil.Units.Physics

--import Control.Lens ((^.))

sspSymbols :: [CQSWrapper]
sspSymbols = (map cqs sspUnits) ++ (map cqs sspUnitless)

sspUnits :: [UCWrapper]
sspUnits = map ucw [normStress, fricAngle, cohesion, dryWeight, satWeight, waterWeight,
              SM.elastMod, coords, waterHght, slopeHght, slipHght, xi, critCoords,
              mobShrI, shrResI, shearFNoIntsl, shearRNoIntsl, slcWght, watrForce,
              watrForceDif, intNormForce, intShrForce, baseHydroForce, surfHydroForce,
              totNrmForce, nrmFSubWat, nrmFNoIntsl, surfLoad, baseAngle, surfAngle,
              impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, genForce,
              momntOfBdy, genDisplace, SM.stffness, shrStiffIntsl, shrStiffBase,
              nrmStiffIntsl, nrmStiffBase, shrStiffRes, nrmStiffRes, shrDispl,
              nrmDispl, dx_i, dy_i, porePressure, elmNrmDispl, elmPrllDispl]

normStress, fricAngle, cohesion, dryWeight, satWeight, waterWeight,
  coords, waterHght, slopeHght, slipHght, xi, critCoords, mobShrI,
  shearFNoIntsl, shearRNoIntsl, slcWght, watrForce, watrForceDif, shrResI,
  intShrForce, baseHydroForce, surfHydroForce, totNrmForce, nrmFSubWat,
  nrmFNoIntsl, surfLoad, baseAngle, surfAngle, impLoadAngle, baseWthX,
  baseLngth, surfLngth, midpntHght, genForce, momntOfBdy, genDisplace,
  shrStiffIntsl, shrStiffBase, nrmStiffIntsl, nrmStiffBase, shrStiffRes,
  nrmStiffRes, shrDispl, nrmDispl, dx_i, dy_i, porePressure, elmNrmDispl,
  elmPrllDispl, intNormForce :: UnitalChunk

--FIXME: Many of these need to be split into term, defn pairs as their defns are
-- mixed into the terms.

fixme, fsi, fisi :: String
fixme = "FIXME: Define this or remove the need for definitions"
fsi = "(for slice index i)"
fisi = "(for interslice index i)"

normStress  = SM.nrmStrss

fricAngle   = uc' "varphi'" (cn $ "effective angle of friction")
  ("The angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (Concat [Greek Phi_V, Atomic "'"]) degree

cohesion    = uc' "c'" (cn $ "effective cohesion")
  fixme
  (Atomic "c'") pascal

dryWeight   = uc' "gamma" (cn $ "dry unit weight")
  "The weight of a dry soil/ground layer divided by the volume of the layer."
  (Greek Gamma_L) specific_weight

satWeight   = uc' "gamma_sat" (cn $ "saturated unit weight")
  "The weight of saturated soil/ground layer divided by the volume of the layer."
  (sub (Greek Gamma_L) (Atomic "Sat")) specific_weight

waterWeight = uc' "gamma_w" (cn $ "unit weight of water")
  "The weight of one cubic meter of water."
  (sub (Greek Gamma_L) lW) specific_weight

--elastMod    = SM.elastMod

coords      = uc' "(x,y)"
  (cn $ "cartesian position coordinates; y is considered parallel to the " ++
  "direction of the force of gravity and x is considered perpendicular to y")
  fixme
  (Atomic "(x,y)") metre

waterHght   = uc' "y_wt,i"
  (cn $ "the y ordinate, or height of the water table at i; refers to either " ++
  "slice i midpoint, or slice interface i")
  fixme
  (sub lY (Atomic "wt,i")) metre

slopeHght   = uc' "y_us,i" (cn $ "the y ordinate, or height of the top of the " ++
  "slope at i; refers to either slice i midpoint, or slice interface i")
  fixme
  (sub lY (Atomic "us,i")) metre

slipHght    = uc' "y_slip,i" (cn $ "the y ordinate, or height of the slip " ++
  "surface at i; refers to either slice i midpoint, or slice interface i")
  fixme
  (sub lY (Atomic "slip,i")) metre

xi          = uc' "x_i"
  (cn $ "the x ordinate; refers to either slice i midpoint, or slice interface i")
  fixme
  (sub lX lI) metre

critCoords  = uc' "(xcs,ycs)" (cn $ "the set of x and y coordinates that " ++
  "describe the vertices of the critical slip surface")
  fixme
  (Concat [sub (Atomic "{x") (Atomic "cs"), sub (Atomic ",y") (Atomic "cs"),
  Atomic "}"]) metre

mobShrI     = uc' "S_i" (cn $ "mobilized shear force for slice i")
  fixme
  (sub cS lI) newton

--mobShear    = SM.mobShear

shrResI     = uc' "P_i" (cn $ "shear resistance; Mohr Coulomb frictional " ++
  "force that describes the limit of mobilized shear force the slice i " ++
  "can withstand before failure")
  fixme
  (sub cP lI) newton

--p           = SM.shearRes

shearFNoIntsl = uc' "T_i"
  (cn $ "mobilized shear force without the influence of interslice forces for slice i")
  fixme
  (sub cT lI) newton

shearRNoIntsl = uc' "R_i"
  (cn $ "shear resistance without the influence of interslice forces for slice i")
  fixme
  (sub cR lI) newton

slcWght     = uc' "W_i" (cn $ "weight; downward force caused by gravity on slice i")
  fixme
  (sub cW lI) newton

watrForce    = uc' "H_i" (cn $ "interslice water force exerted in the " ++
  "x-ordinate direction between adjacent slices " ++ fisi)
  fixme
  (sub cH lI) newton

watrForceDif = uc' "dH_i" (cn $ "difference between interslice forces on acting in the x-ordinate direction of the slice on each side " ++ fisi)
  fixme
  (sub (Concat [Greek Delta, cH]) lI) newton

intNormForce = uc' "E_i" (cn $ "interslice normal force being exerted between adjacent slices " ++ fisi)
  fixme
  (sub cE lI) newton

intShrForce = uc' "X_i" (cn $ "interslice shear force being exerted between adjacent slices " ++ fisi)
  fixme
  (sub cX lI) newton

baseHydroForce = uc' "U_b,i" (cn $ "base hydrostatic force arising from water pressure within the slice " ++ fsi)
  fixme
  (sub cU (Atomic "b,i")) newton

surfHydroForce = uc' "U_t,i" (cn $ "surface hydrostatic force arising from water pressure acting into the slice from standing water on the slope surface " ++ fsi)
  fixme
  (sub cU (Atomic "t,i")) newton

totNrmForce = uc' "N_i" (cn $ "total reactive force for a soil surface subject to a body resting on it")
  fixme
  (sub cN lI) newton

nrmFSubWat = uc' "N'_i" (cn $ "effective normal force of a soil surface, subtracting pore water reactive force from total reactive force")
  fixme
  (sub (Atomic "N'") lI) newton

nrmFNoIntsl = uc' "N*_i" (cn $ "effective normal force of a soil surface, neglecting the influence of interslice forces")
  fixme
  (sub (Atomic "N*") lI) newton

surfLoad    = uc' "Q_i" (cn $ "imposed surface load; a downward force acting into the surface from midpoint of slice i")
  fixme
  (sub cQ lI) newton

baseAngle   = uc' "alpha_i" (cn $ "angle of the base of the mass relative to the horizontal " ++ fsi)
  fixme
  (sub (Greek Alpha_L) lI) degree

surfAngle   = uc' "beta_i" (cn $ "angle of the surface of the mass relative to the horizontal " ++ fsi)
  fixme
  (sub (Greek Beta_L) lI) degree

impLoadAngle = uc' "omega_i" (cn $ "angle of imposed surface load acting into the surface relative to the vertical " ++ fsi)
  fixme
  (sub (Greek Omega_L) lI) degree

baseWthX    = uc' "b_i" (cn $ "base width of the slice in the x-ordinate direction only " ++ fsi)
  fixme
  (sub lB lI) metre

baseLngth   = uc' "l_b,i" (cn $ "total length of the base of a slice " ++ fsi)
  fixme
  (sub (Greek Ell) (Atomic "b,i")) metre

surfLngth   = uc' "l_s,i" (cn $ "length of an interslice surface, from slip base to slope surface in a vertical line from an interslice vertex " ++ fisi)
  fixme
  (sub (Greek Ell) (Atomic "s,i")) metre

midpntHght  = uc' "h_i" (cn $ "midpoint height; distance from the slip base to the slope surface in a vertical line from the midpoint of the slice (for slice i)")
  fixme
  (sub lH lI) metre

genForce = uc CP.force cF newton

-- genForce    = uc' "F" (cn $ "generic force; assumed 1D allowing a scalar")
  -- fixme
  -- cF newton --FIXME: use force from concepts.physics or quantity.physics ?

momntOfBdy  = uc' "M" (cn $ "moment of a body; assumed 2D allowing a scalar")
  fixme
  cM momentOfForceU --FIXME: move in concepts.physics ?

genDisplace = uc' "genDisplace" (cn $ "generic displacement of a body")
  fixme
  (Greek Delta_L) metre

--k           = SM.stffness

shrStiffIntsl = uc' "K_st,i" (cn $ "shear stiffness of an interslice surface, without length adjustment " ++ fisi)
  fixme
  (sub cK (Atomic "st,i")) pascal

shrStiffBase  = uc' "K_bt,i" (cn $ "shear stiffness of a slice base surface, without length adjustment " ++ fsi)
  fixme
  (sub cK (Atomic "bt,i")) pascal

nrmStiffIntsl = uc' "K_sn,i" (cn $ "normal stiffness of an interslice surface, without length adjustment " ++ fisi)
  fixme
  (sub cK (Atomic "sn,i")) pascal

nrmStiffBase = uc' "K_bn,i" (cn $ "normal stiffness of a slice base surface, without length adjustment " ++ fsi)
  fixme
  (sub cK (Atomic "bn,i")) pascal

shrStiffRes  = uc' "K_tr" (cn $ "residual shear stiffness")
  fixme
  (sub cK (Atomic "tr")) pascal

nrmStiffRes  = uc' "K_no" (cn $ "residual normal stiffness")
  fixme
  (sub cK (Atomic "no")) pascal

shrDispl = uc' "du_i" (cn $ "shear displacement of a slice " ++ fsi)
  fixme
  (sub (Concat [Greek Delta_L, Atomic "u"]) lI) metre

nrmDispl = uc' "dv_i" (cn $ "normal displacement of a slice " ++ fsi)
  fixme
  (sub (Concat [Greek Delta_L, Atomic "v"]) lI) metre
  
elmNrmDispl  = uc' "dt_i" (cn $ "displacement of the element normal to the surface " ++ fsi)
  fixme
  (sub (Concat [Greek Delta_L, Atomic "t"]) lI) metre
  
elmPrllDispl = uc' "dn_i" (cn $ "displacement of the element parallel to the surface " ++ fsi)
  fixme
  (sub (Concat [Greek Delta_L, Atomic "n"]) lI) metre

dx_i        = uc' "dx_i" (cn $ "displacement of a slice in the x-ordinate direction " ++ fsi)
  fixme
  (sub (Concat [Greek Delta_L, Atomic "x"]) lI) metre

dy_i        = uc' "dy_i" (cn $ "displacement of a slice in the y-ordinate direction " ++ fsi)
  fixme
  (sub (Concat [Greek Delta_L, Atomic "y"]) lI) metre

porePressure = uc' "mu" (cn "pore pressure from water within the soil") fixme
  (Greek Mu_L) pascal


-- Unitless Symbols --

sspUnitless :: [ConVar]
sspUnitless = [SM.poissnsR, fs, earthqkLoadFctr, normToShear, scalFunc, numbSlices, minFunction, fsloc]

fs, earthqkLoadFctr, normToShear, scalFunc, numbSlices, minFunction, fsloc :: ConVar

--poisson     = SM.poissnsR

fs          = cvR (dcc "FS" (nounPhraseSP $ "global factor of safety describing the " ++
  "stability of a surface in a slope") fixme) (Atomic "FS")

earthqkLoadFctr = cvR (dcc "K_c" (nounPhraseSP $ "earthquake load factor; proportionality " ++
  "factor of force that weight pushes outwards; caused by seismic earth movements")
  fixme) (sub cK lC)

normToShear = cvR (dcc "lambda" (nounPhraseSP $ "ratio between interslice normal and " ++
  "shear forces (applied to all interslices)") fixme) (Greek Lambda_L)

scalFunc    = cvR (dcc "f_i" (nounPhraseSP $ "scaling function for magnitude of interslice " ++
  "forces as a function of the x coordinate (at interslice index i); can be constant or a half-sine")
  fixme) (sub lF lI)

numbSlices  = cvRs (dcc "n" (nounPhraseSP "number of slices the slip mass has been divided into")
  fixme) lN Natural

minFunction = cvR (dcc "Upsilon" (nounPhraseSP "generic minimization function or algorithm")
  fixme) (Greek Upsilon)

fsloc       = cvR (dcc "FS_loci" (nounPhraseSP "local factor of safety specific to a slice i")
  fixme) (sub (Atomic "FS") (Atomic "Loc,i"))