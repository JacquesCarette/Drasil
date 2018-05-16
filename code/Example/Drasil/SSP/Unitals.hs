module Drasil.SSP.Unitals where --export all of it

import Language.Drasil

import Data.Drasil.SI_Units (newton, pascal, metre, degree, specific_weight)
import Data.Drasil.Units.SolidMechanics (stiffness3D)
import Data.Drasil.Quantities.Physics as QP (force, pressure)
import Data.Drasil.Quantities.SolidMechanics as SM (nrmStrss, elastMod,
  poissnsR, stffness)
import Data.Drasil.Units.Physics (momentOfForceU)
import Drasil.SSP.Defs (fs_concept)
import Data.Drasil.Constraints (gtZeroConstr)


sspSymbols :: [DefinedQuantityDict]
sspSymbols = (map cqs sspInputs) ++ (map cqs sspOutputs) ++
  (map cqs sspUnits) ++ (map cqs sspUnitless)

---------------------------
-- Imported UnitalChunks --
---------------------------
{-
SM.mobShear, SM.shearRes <- currently not used
SM.poissnsR, SM.elastMod <- Used to make UncertQ
-}
normStress  = SM.nrmStrss
genForce = uc QP.force cF newton --must import from Concept.Physics
                                 --since force is a vector otherwise
genPressure = QP.pressure
genStffness = SM.stffness

-------------
-- HELPERS --
-------------
fixme, fsi, fisi, wiif, wla, smsi :: String
fixme = "FIXME: missing discription"
fsi   = "for slice index i"
fisi  = "for interslice index i"
wiif  = "without the influence of interslice forces"
wla   = "without length adjustment"
smsi  = "refers to either slice i midpoint, or slice interface i"

--------------------------------
-- START OF CONSTRAINEDCHUNKS --
--------------------------------

sspConstrained :: [ConstrainedChunk]
sspConstrained = map cnstrw sspInputs ++ map cnstrw sspOutputs

sspInputs :: [UncertQ]
sspInputs = [elasticMod, cohesion, poissnsRatio, fricAngle, dryWeight,
              satWeight, waterWeight, constant_a, constant_A, constant_K]

sspOutputs :: [ConstrConcept]
sspOutputs = [fs, coords, dx_i, dy_i]

{-
monotonicIn :: [Constraint]  --FIXME: Move this?
monotonicIn = [physc $ \_ -> -- FIXME: Hack with "index" !
  (idx xi (sy index) $< idx xi (sy index + 1) $=> idx yi (sy index) $< idx yi (sy index + 1))]
-}

defultUncrt :: Double
defultUncrt = 0.1

elasticMod, cohesion, poissnsRatio, fricAngle, dryWeight, satWeight,
  waterWeight, constant_a, constant_A, constant_K :: UncertQ
  
fs, coords, dx_i, dy_i :: ConstrConcept

{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals

elasticMod = uq (constrained' SM.elastMod [gtZeroConstr]
  (dbl 15000)) defultUncrt

cohesion = uqc "c'" (cn $ "effective cohesion")
  "internal pressure that sticks particles of soil together"
  (prime $ Atomic "c") pascal Real [gtZeroConstr] (dbl 10) defultUncrt

poissnsRatio = uq (constrained' SM.poissnsR
  [physc $ Bounded (Exc,0) (Exc,1)] (dbl 0.4)) defultUncrt

fricAngle = uqc "varphi'" (cn $ "effective angle of friction")
  ("The angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime $ vPhi) degree Real [physc $ Bounded (Exc,0) (Exc,90)]
  (dbl 25) defultUncrt

dryWeight = uqc "gamma" (cn $ "dry unit weight")
  "The weight of a dry soil/ground layer divided by the volume of the layer."
  (lGamma) specific_weight Real [gtZeroConstr]
  (dbl 20) defultUncrt

satWeight = uqc "gamma_sat" (cn $ "saturated unit weight")
  ("The weight of saturated soil/ground " ++
  "layer divided by the volume of the layer.")
  (sub (lGamma) (Atomic "Sat")) specific_weight Real [gtZeroConstr]
  (dbl 20) defultUncrt

waterWeight = uqc "gamma_w" (cn $ "unit weight of water")
  "The weight of one cubic meter of water."
  (sub (lGamma) lW) specific_weight Real [gtZeroConstr]
  (dbl 9.8) defultUncrt
  
constant_a = uqc "a" (cn "constant") fixme
  lA metre Real [] (dbl 0) defultUncrt
  
constant_A = uqc "A" (cn "constant") fixme
  cA metre Real [] (dbl 0) defultUncrt
  
constant_K = uqc "kappa" (cn "constant") fixme
  (lKappa) pascal Real [] (dbl 0) defultUncrt

{-Output Variables-} --FIXME: See if there should be typical values
fs = constrained' (cv fs_concept (Atomic "FS") Real) [gtZeroConstr] (dbl 1)

fs_min :: ConVar -- This is a hack to remove the use of indexing for 'min'.
fs_min = cv (dcc "fs_min" (cn "minimum factor of safety") 
  ("The minimum factor of safety")) (sub (eqSymb fs) (Atomic "min")) Real
-- Once things are converted to the new style of instance models, this will
-- be removed/fixed.

coords = cuc' "(x,y)"
  (cn $ "cartesian position coordinates" )
  ("y is considered parallel to the direction of the force of " ++
  "gravity and x is considered perpendicular to y")
  (Atomic "(x,y)") metre Real [] (dbl 1)

dx_i = cuc' "dx_i" (cn $ "displacement") ("in the x-ordinate direction " ++
  fsi) (Concat [lDelta, Atomic "x"]) metre Real [] (dbl 1)

dy_i = cuc' "dy_i" (cn $ "displacement") ("in the y-ordinate direction " ++
  fsi) (Concat [lDelta, Atomic "y"]) metre Real [] (dbl 1)

---------------------------
-- START OF UNITALCHUNKS --
---------------------------

sspUnits :: [UnitaryConceptDict]
sspUnits = map ucw [normStress, genPressure, normFunc, shearFunc,
  waterHght, slopeHght, slipHght, xi, yi, critCoords, slopeDist, slipDist,
  mobShrI, shrResI, shearFNoIntsl, shearRNoIntsl, slcWght, watrForce,
  watrForceDif, intShrForce, baseHydroForce, surfHydroForce, effStiffA,
  effStiffB, totNrmForce, nrmFSubWat, nrmFNoIntsl, surfLoad, baseAngle,
  surfAngle, impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght,
  genForce, momntOfBdy, genDisplace, genStffness, shrStiffIntsl,
  shrStiffBase, nrmStiffIntsl, nrmStiffBase, shrStiffRes, nrmStiffRes,
  shrDispl, nrmDispl, porePressure, elmNrmDispl, elmPrllDispl, sliceHght,
  fx, fy, mobShrC, shrResC, rotatedDispl, intNormForce, shrStress, mobStress]

normStress, genPressure, normFunc, shearFunc, slopeDist, slipDist, genStffness,
  waterHght, slopeHght, slipHght, xi, yi, critCoords, mobShrI, sliceHght,
  shearFNoIntsl, shearRNoIntsl, slcWght, watrForce, watrForceDif, shrResI,
  intShrForce, baseHydroForce, surfHydroForce, totNrmForce, nrmFSubWat,
  nrmFNoIntsl, surfLoad, baseAngle, surfAngle, impLoadAngle, baseWthX,
  effStiffA, effStiffB, baseLngth, surfLngth, midpntHght, genForce,
  momntOfBdy, genDisplace, fx, fy, shrStiffIntsl, shrStiffBase,
  nrmStiffIntsl, nrmStiffBase, shrStiffRes, nrmStiffRes, shrDispl, nrmDispl,
  porePressure, elmNrmDispl, mobStress, elmPrllDispl, mobShrC, shrResC,
  rotatedDispl, intNormForce, shrStress :: UnitalChunk
  
{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = uc' "E_i" (cn $ "interslice normal force")
  ("exerted between adjacent slices " ++ fisi)
  (cE) newton

waterHght = uc' "y_wt,i"
  (cn $ "y ordinate")
  ("height of the water table at i, " ++ smsi)
  (sub lY (Atomic "wt")) metre

slopeHght = uc' "y_us,i" (cn $ "y ordinate")
  ("height of the top of the slope at i, " ++ smsi)
  (sub lY (Atomic "us")) metre

slipHght = uc' "y_slip,i" (cn $ "y ordinate")
  ("height of the slip surface at i, " ++ smsi)
  (sub lY (Atomic "slip")) metre

slopeDist = uc' "x_us,i" (cn $ "x ordinate")
  ("distance of the edge of the slope at i, " ++ smsi)
  (sub lX (Atomic "us")) metre 

slipDist = uc' "x_slip,i" (cn $ "x ordinate")
  ("distance of the slip surface at i, " ++ smsi)
  (sub lX (Atomic "slip")) metre

yi = uc' "y_i" (cn $ "y ordinate") smsi lY metre
  
xi = uc' "x_i" (cn $ "x ordinate") smsi lX metre

critCoords = uc' "(xcs,ycs)" (cn $ "the set of x and y coordinates")
  "describe the vertices of the critical slip surface"
  (sCurlyBrSymb (Concat [sub (Atomic "x") (Atomic "cs"),
  sub (Atomic ",y") (Atomic "cs")])) metre

mobShrI = uc' "mobShear" (cn $ "mobilized shear force")
  fsi
  (cS) newton --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.mobShear
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shrResI = uc' "shearRes" (cn $ "resistive shear force") ("Mohr Coulomb " ++
  "frictional force that describes the limit of mobilized shear force the " ++
  "slice i can withstand before failure")
  (cP) newton --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.shearRes
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.
  
mobShrC = uc' "Psi" (cn $ "constant") ("converts mobile shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces")
  (Greek Psi) newton

shrResC = uc' "Phi" (cn $ "constant") ("converts resistive shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces")
  cPhi newton

shearFNoIntsl = uc' "T_i"
  (cn $ "mobilized shear force") (wiif ++ " " ++ fsi)
  cT newton

shearRNoIntsl = uc' "R_i"
  (cn $ "resistive shear force") (wiif ++ " " ++ fsi)
  (cR) newton

slcWght = uc' "W_i" (cn $ "weight")
  ("downward force caused by gravity on slice i") (cW) newton

watrForce = uc' "H_i" (cn $ "interslice water force") ("exerted in the " ++
  "x-ordinate direction between adjacent slices " ++ fisi)
  (cH) newton

watrForceDif = uc' "dH_i" (cn $ "difference between interslice forces")
  ("exerted in the x-ordinate direction between adjacent slices " ++ fisi)
  (Concat [cDelta, cH]) newton

intShrForce = uc' "X_i" (cn $ "interslice shear force") 
  ("exerted between adjacent slices " ++ fisi)
  (cX) newton

baseHydroForce = uc' "U_b,i" (cn $ "base hydrostatic force")
  ("from water pressure within the slice " ++ fsi)
  (sub cU (Atomic "b")) newton

surfHydroForce = uc' "U_t,i" (cn $ "surface hydrostatic force")
  ("from water pressure acting into the slice from standing " ++
  "water on the slope surface " ++ fsi)
  (sub cU (Atomic "t")) newton

totNrmForce = uc' "N_i" (cn $ "normal force") ("total reactive force " ++
  "for a soil surface subject to a body resting on it")
  cN newton

nrmFSubWat = uc' "N'_i" (cn $ "effective normal force")
  ("for a soil surface, subtracting pore water reactive force from total " ++
  "reactive force") (prime $ Atomic "N") newton

nrmFNoIntsl = uc' "N*_i" (cn $ "effective normal force")
  ("for a soil surface, " ++ wiif) (Atomic "N*") newton

surfLoad = uc' "Q_i" (cn $ "imposed surface load") 
  "a downward force acting into the surface from midpoint of slice i"
  (cQ) newton

baseAngle = uc' "alpha_i" (cn $ "angle")
  ("base of the mass relative to the horizontal " ++ fsi)
  (lAlpha) degree

surfAngle = uc' "beta_i" (cn $ "angle")
  ("surface of the mass relative to the horizontal " ++ fsi)
  (lBeta) degree

impLoadAngle = uc' "omega_i" (cn $ "angle")
  ("of imposed surface load acting into the surface " ++
  "relative to the vertical " ++ fsi) (Greek Omega_L) degree

baseWthX = uc' "b_i" (cn $ "base width of a slice")
  ("in the x-ordinate direction only " ++ fsi)
  (lB) metre

baseLngth = uc' "l_b,i" (cn $ "total base length of a slice") fsi
  (sub (Greek Ell) (Atomic "b")) metre

surfLngth = uc' "l_s,i" (cn $ "length of an interslice surface")
  ("from slip base to slope surface in a vertical " ++
  "line from an interslice vertex " ++ fisi)
  (sub (Greek Ell) (Atomic "s")) metre

midpntHght = uc' "h_i" (cn $ "midpoint height")
  ("distance from the slip base to the slope surface in a vertical " ++
  "line from the midpoint of the slice " ++ fsi)
  (lH) metre

momntOfBdy = uc' "M" (cn $ "moment of a body") ("assumed 2D allowing a scalar")
  cM momentOfForceU --FIXME: move in concepts.physics ?

genDisplace = uc' "genDisplace" (cn $ "displacement")
  "generic displacement of a body" (lDelta) metre

shrStiffIntsl = uc' "K_st,i" (cn $ "shear stiffness")
  ("for interslice surface, " ++ wla ++ " " ++ fisi)
  (sub cK (Atomic "st")) stiffness3D

shrStiffBase = uc' "K_bt,i" (cn $ "shear stiffness") 
  ("for a slice base surface, " ++ wla ++ " " ++ fsi)
  (sub cK (Atomic "bt")) stiffness3D

nrmStiffIntsl = uc' "K_sn,i" (cn $ "normal stiffness")
  ("for an interslice surface, " ++ wla ++ " " ++ fisi)
  (sub cK (Atomic "sn")) stiffness3D

nrmStiffBase = uc' "K_bn,i" (cn $ "normal stiffness") 
  ("for a slice base surface, " ++ wla ++ " " ++ fsi)
  (sub cK (Atomic "bn")) stiffness3D

shrStiffRes = uc' "K_tr" (cn $ "shear stiffness")
  "residual strength"
  (sub cK (Atomic "tr")) stiffness3D

nrmStiffRes = uc' "K_no" (cn $ "normal stiffness")
  "residual strength"
  (sub cK (Atomic "no")) stiffness3D

effStiffA = uc' "K_bA" (cn $ "effective base stiffness A")
  ("for rotated coordinates of a slice base surface, " ++ fsi)
  (sub cK (Atomic "bA")) stiffness3D

effStiffB = uc' "K_bB" (cn $ "effective base stiffness A")
  ("for rotated coordinates of a slice base surface, " ++ fsi)
  (sub cK (Atomic "bB")) stiffness3D

shrDispl = uc' "du_i" (cn $ "displacement")
  ("shear displacement " ++ fsi)
  (Concat [lDelta, Atomic "u"]) metre

nrmDispl = uc' "dv_i" (cn $ "displacement")
  ("normal displacement " ++ fsi)
  (Concat [lDelta, Atomic "v"]) metre
  
elmNrmDispl = uc' "dt_i" (cn $ "displacement")
  ("for the element normal to the surface " ++ fsi)
  (Concat [lDelta, Atomic "t"]) metre
  
elmPrllDispl = uc' "dn_i" (cn $ "displacement")
  ("for the element parallel to the surface " ++ fsi)
  (Concat [lDelta, Atomic "n"]) metre

porePressure = uc' "mu" (cn "pore pressure") ("from water within the soil")
  (lMu) pascal

rotatedDispl = uc' "varepsilon_i" (cn "displacement")
  ("in rotated coordinate system")
  (vEpsilon) metre
  
shrStress = uc' "tau_i" (cn "resistive shear stress")
  ("acting on the base of a slice")
  (lTau) pascal
  
mobStress = uc' "s_i" (cn "mobilized shear stress")
  ("acting on the base of a slice")
  (lS) pascal

sliceHght = uc' "z_i" (cn "center of slice height")
  ("the distance from the lowest part " ++
  "of the slice to the height of the centers of slice")
  (lZ) metre

normFunc = uc' "C1_i" (cn "interslice normal force function")
  (fixme)
  (Concat [cC, Atomic "1"]) momentOfForceU
  
shearFunc = uc' "C2_i" (cn "interslice shear force function")
  (fixme)
  (Concat [cC, Atomic "2"]) momentOfForceU

fx = uc' "fx" (cn "x-component of the net force") fixme
  (sub cF lX) newton

fy = uc' "fy" (cn "y-component of the net force") fixme
  (sub cF lY) newton
  
----------------------
-- Unitless Symbols --
----------------------

sspUnitless :: [ConVar]
sspUnitless = [earthqkLoadFctr, normToShear,scalFunc,
  numbSlices, minFunction, fsloc, index, varblU, varblV, fs_min,
  ufixme1, ufixme2]

earthqkLoadFctr, normToShear, scalFunc,
  numbSlices, minFunction, fsloc, index, varblU, varblV :: ConVar

earthqkLoadFctr = cv (dcc "K_c" (nounPhraseSP $ "earthquake load factor")
  ("proportionality factor of force that " ++
  "weight pushes outwards; caused by seismic earth movements"))
  (sub cK lC) Real

normToShear = cv (dcc "lambda"
  (nounPhraseSP $ "interslice normal/shear force ratio")
  ("applied to all interslices")) (lLambda) Real

scalFunc = cv (dcc "f_i" (nounPhraseSP $ "scaling function")
  ("magnitude of interslice forces as a function " ++
  "of the x coordinate" ++ fisi ++ "; can be constant or a half-sine"))
  (lF) Real

numbSlices = cv (dcc "n" (nounPhraseSP "number of slices")
  "the slip mass has been divided into")
  lN Natural

minFunction = cv (dcc "Upsilon" (nounPhraseSP "function")
  ("generic minimization function or algorithm"))
  (cUpsilon) Real

fsloc = cv (dcc "FS_loci" (nounPhraseSP "local factor of safety") fsi)
  (sub (Atomic "FS") (Atomic "Loc,i")) Real

ufixme1 = cv (dcc "fixme1" (cn "fixme") "What is this value?")
  (Atomic "SpencerFixme1Please") Real

ufixme2 = cv (dcc "fixme2" (cn "fixme") "What is this value?")
  (Atomic "SpencerFixme2Please") Real

--------------------
-- Index Function --
--------------------

varblU = cv (dcc "varblU" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  lU Natural
varblV = cv (dcc "varblV" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  lV Natural

index = cv (dcc "index" (nounPhraseSP "index")
  ("used to show a quantity applies to only one slice")) lI Natural

--FIXME: possibly move to Language/Drasil/Expr.hs
indx1 :: (Quantity a) => a -> Expr
indx1 a = idx (sy a) 1

indxn :: (Quantity a) => a -> Expr
indxn a = idx (sy a) (sy numbSlices)

inxi, inxiP1, inxiM1 :: Quantity e => e -> Expr
inxiP1 e = inx e 1
inxi   e = inx e 0
inxiM1 e = inx e (-1)

inx :: Quantity e => e -> Integer -> Expr
inx e n 
  | n < 0     = idx (sy e) (sy index - int (-n))
  | n == 0    = idx (sy e) (sy index)
  | otherwise = idx (sy e) (sy index + int n)

sum1toN :: Expr -> Expr
sum1toN = defsum (eqSymb index) 1 (sy numbSlices)
