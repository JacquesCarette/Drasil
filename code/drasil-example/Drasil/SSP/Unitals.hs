module Drasil.SSP.Unitals where --export all of it

import Language.Drasil
import Language.Drasil.ShortHands

import Drasil.SSP.Defs (fs_concept)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, newton, pascal, specific_weight)

import Data.Drasil.Units.Physics (momentOfForceU)

import Data.Drasil.Quantities.Math (area)
import Data.Drasil.Quantities.Physics (force)
import Data.Drasil.Quantities.SolidMechanics as SM (nrmStrss)


sspSymbols :: [DefinedQuantityDict]
sspSymbols = (map dqdWr sspInputs) ++ (map dqdWr sspOutputs) ++
  (map dqdWr sspUnits) ++ (map dqdWr sspUnitless)

---------------------------
-- Imported UnitalChunks --
---------------------------
{-
SM.mobShear, SM.shearRes <- currently not used
SM.poissnsR, SM.elastMod <- Used to make UncertQ
-}
normStress  = SM.nrmStrss
genericF = force
genericA = area

-------------
-- HELPERS --
-------------
fixme, fsi, fisi, wiif, wla, smsi :: String
fixme = "FIXME: missing description"
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
sspInputs = [cohesion, fricAngle, dryWeight, satWeight, waterWeight]

sspOutputs :: [ConstrConcept]
sspOutputs = [fs, coords]

{-
monotonicIn :: [Constraint]  --FIXME: Move this?
monotonicIn = [physc $ \_ -> -- FIXME: Hack with "index" !
  (idx xi (sy index) $< idx xi (sy index + 1) $=> idx yi (sy index) $< idx yi (sy index + 1))]
-}

defultUncrt :: Double
defultUncrt = 0.1

cohesion, fricAngle, dryWeight, satWeight,
  waterWeight :: UncertQ
  
fs, coords :: ConstrConcept

{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals

cohesion = uqc "c'" (cn $ "effective cohesion")
  "internal pressure that sticks particles of soil together"
  (prime $ Atomic "c") pascal Real [gtZeroConstr] (dbl 10) defultUncrt

fricAngle = uqc "varphi'" (cn $ "effective angle of friction")
  ("The angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime $ vPhi) degree Real [physc $ Bounded (Exc,0) (Exc,90)]
  (dbl 25) defultUncrt

dryWeight = uqc "gamma" (cn $ "dry unit weight")
  "The weight of a dry soil/ground layer divided by the volume of the layer."
  lGamma specific_weight Real [gtZeroConstr]
  (dbl 20) defultUncrt

satWeight = uqc "gamma_sat" (cn $ "saturated unit weight")
  ("The weight of saturated soil/ground " ++
  "layer divided by the volume of the layer.")
  (sub lGamma (Atomic "Sat")) specific_weight Real [gtZeroConstr]
  (dbl 20) defultUncrt

waterWeight = uqc "gamma_w" (cn $ "unit weight of water")
  "The weight of one cubic meter of water."
  (sub lGamma lW) specific_weight Real [gtZeroConstr]
  (dbl 9.8) defultUncrt

{-Output Variables-} --FIXME: See if there should be typical values
fs = constrained' (dqd' fs_concept (const $ sub cF (Atomic "S")) Real Nothing)
  [gtZeroConstr] (dbl 1)

fs_min :: DefinedQuantityDict -- This is a hack to remove the use of indexing for 'min'.
fs_min = dqd' (dcc "fs_min" (cn "minimum factor of safety") 
  ("The minimum factor of safety")) (const $ sub (eqSymb fs) (Atomic "min")) Real
  Nothing 
-- Once things are converted to the new style of instance models, this will
-- be removed/fixed.

coords = cuc' "(x,y)"
  (cn $ "cartesian position coordinates" )
  ("y is considered parallel to the direction of the force of " ++
  "gravity and x is considered perpendicular to y")
  (Atomic "(x,y)") metre Real [] (dbl 1)

---------------------------
-- START OF UNITALCHUNKS --
---------------------------

sspUnits :: [UnitaryConceptDict]
sspUnits = map ucw [normStress, genericF, genericA, normFunc, shearFunc,
  waterHght, slopeHght, slipHght, xi, yi, critCoords, slopeDist, slipDist,
  mobShrI, shrResI, shearFNoIntsl, shearRNoIntsl, slcWght, watrForce,
  watrForceDif, intShrForce, baseHydroForce, surfHydroForce, totNrmForce, nrmFSubWat, nrmFNoIntsl, surfLoad, baseAngle,
  surfAngle, impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght,
  momntOfBdy, porePressure, sliceHght,
  fx, fy, nrmForceSum, watForceSum, mobShrC, shrResC, intNormForce, shrStress]

normStress, genericF, genericA, normFunc, shearFunc, slopeDist, slipDist,
  waterHght, slopeHght, slipHght, xi, yi, critCoords, mobShrI, sliceHght,
  shearFNoIntsl, shearRNoIntsl, slcWght, watrForce, watrForceDif, shrResI,
  intShrForce, baseHydroForce, surfHydroForce, totNrmForce, nrmFSubWat,
  nrmFNoIntsl, surfLoad, baseAngle, surfAngle, impLoadAngle, baseWthX,
  baseLngth, surfLngth, midpntHght,
  momntOfBdy, fx, fy, nrmForceSum, watForceSum,
  porePressure, mobShrC, shrResC,
  intNormForce, shrStress :: UnitalChunk
  
{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = uc' "G_i" (cn $ "interslice normal force")
  ("exerted between adjacent slices " ++ fisi)
  (cG) newton

waterHght = uc' "y_wt,i"
  (cn $ "y ordinate")
  ("height of the water table at i, " ++ smsi)
  (sub lY (Atomic "wt")) metre

slopeHght = uc' "y_slope,i" (cn $ "slope y-ordinate")
  ("y-ordinate of a point on the slope")
  (sub lY (Atomic "slope")) metre

slipHght = uc' "y_slip,i" (cn $ "y ordinate")
  ("height of the slip surface at i, " ++ smsi)
  (sub lY (Atomic "slip")) metre

slopeDist = uc' "x_slope,i" (cn $ "slope x-ordinate")
  ("x-ordinate of a point on the slope")
  (sub lX (Atomic "slope")) metre

slipDist = uc' "x_slip,i" (cn $ "x ordinate")
  ("distance of the slip surface at i, " ++ smsi)
  (sub lX (Atomic "slip")) metre

yi = uc' "y_i" (cn $ "y ordinate") smsi lY metre
  
xi = uc' "x_i" (cn $ "x ordinate") smsi lX metre

-- FIXME: the 'symbol' for this should not have { and } embedded in it.
-- They have been removed now, but we need a reasonable notation.
critCoords = uc' "(xcs,ycs)" (cn $ "the set of x and y coordinates")
  "describe the vertices of the critical slip surface"
  (Concat [sub (Atomic "x") (Atomic "cs"), Atomic ",",
  sub (Atomic "y") (Atomic "cs")]) metre

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
  
mobShrC = uc' "Psi" (cn $ "second function for incorporating interslice " ++
  "forces into shear force") ("converts mobile shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces")
  cPsi newton

shrResC = uc' "Phi" (cn $ "first function for incorporating interslice " ++
  "forces into shear force") ("converts resistive shear " ++ 
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
  lAlpha degree

surfAngle = uc' "beta_i" (cn $ "angle")
  ("surface of the mass relative to the horizontal " ++ fsi)
  lBeta degree

impLoadAngle = uc' "omega_i" (cn $ "angle")
  ("of imposed surface load acting into the surface " ++
  "relative to the vertical " ++ fsi) lOmega degree

baseWthX = uc' "b_i" (cn $ "base width of a slice")
  ("in the x-ordinate direction only " ++ fsi)
  (lB) metre

baseLngth = uc' "l_b,i" (cn $ "total base length of a slice") fsi
  (sub lEll (Atomic "b")) metre

surfLngth = uc' "l_s,i" (cn $ "length of an interslice surface")
  ("from slip base to slope surface in a vertical " ++
  "line from an interslice vertex " ++ fisi)
  (sub lEll (Atomic "s")) metre

midpntHght = uc' "h_i" (cn $ "y-direction height of a slice")
  ("height in the y-direction from the base of a slice to the slope " ++
  "surface, at the x-direction midpoint of the slice")
  (lH) metre

momntOfBdy = uc' "M" (cn $ "moment") ("a measure of the tendency of " ++
  "a body to rotate about a specific point or axis")
  cM momentOfForceU --FIXME: move in concepts.physics ?

porePressure = uc' "mu" (cn "pore pressure") ("from water within the soil")
  lMu pascal
  
shrStress = uc' "tau_i" (cn "resistive shear stress")
  ("acting on the base of a slice")
  lTau pascal

sliceHght = uc' "z_i" (cn "center of slice height")
  ("the distance from the lowest part " ++
  "of the slice to the height of the centers of slice")
  (lZ) metre

normFunc = uc' "C1_i" (cn "interslice normal force function")
  "the normal force at the interslice interface for slice i"
  (sub (Concat [cC, Atomic "1"]) lI) momentOfForceU
  
shearFunc = uc' "C2_i" (cn "interslice shear force function")
  "the shear force at the interslice interface for slice i"
  (sub (Concat [cC, Atomic "2"]) lI) momentOfForceU

fx = uc' "fx" (cn "x-component of the net force") ""
  (sub cF lX) newton

fy = uc' "fy" (cn "y-component of the net force") ""
  (sub cF lY) newton

nrmForceSum = uc' "F_x^G" (cn "sum of the interslice normal forces") ""
  (sup (sub cF lX) cG) newton

watForceSum = uc' "F_x^H" (cn "sum of the interslice water forces") ""
  (sup (sub cF lX) cH) newton
  
----------------------
-- Unitless Symbols --
----------------------

sspUnitless :: [DefinedQuantityDict]
sspUnitless = [constF, earthqkLoadFctr, normToShear, scalFunc,
  numbSlices, minFunction, index, varblU, varblV, fs_min,
  ufixme3, ufixme4]

constF, earthqkLoadFctr, normToShear, scalFunc, numbSlices,
  minFunction, index, varblU, varblV, ufixme3, ufixme4 :: DefinedQuantityDict

constF = dqd' (dcc "const_f" (nounPhraseSP $ "decision on f") 
  ("boolean decision on which form of f the user desires: constant if true," ++
  " or half-sine if false")) (const (Atomic "const_f")) Boolean Nothing

earthqkLoadFctr = dqd' (dcc "K_c" (nounPhraseSP $ "earthquake load factor")
  ("proportionality factor of force that " ++
  "weight pushes outwards; caused by seismic earth movements"))
  (const $ sub cK lC) Real Nothing 

normToShear = dqd' (dcc "lambda"
  (nounPhraseSP $ "interslice normal/shear force ratio")
  ("applied to all interslices")) (const lLambda) Real Nothing

scalFunc = dqd' (dcc "f_i" (nounPhraseSP $ "interslice normal to shear " ++
  "force ratio variation function")
  ("magnitude of interslice forces as a function " ++
  "of the x coordinate" ++ fisi ++ "; can be constant or a half-sine"))
  (const lF) Real Nothing 

numbSlices = dqd' (dcc "n" (nounPhraseSP "number of slices")
  "the slip mass has been divided into")
  (const lN) Natural Nothing

minFunction = dqd' (dcc "Upsilon" (nounPhraseSP "function")
  ("generic minimization function or algorithm"))
  (const cUpsilon) Real Nothing

ufixme3 = dqd' (dcc "fixme3" (cn "fixme") "What is this value?")
  (const $ Atomic "SpencerFixme3Please") Real Nothing

ufixme4 = dqd' (dcc "fixme4" (cn "fixme") "What is this value?")
  (const $ Atomic "SpencerFixme4Please") Real Nothing 

--------------------
-- Index Function --
--------------------

varblU = dqd' (dcc "varblU" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  (const lU) Natural Nothing 
varblV = dqd' (dcc "varblV" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  (const lV) Natural Nothing

index = dqd' (dcc "index" (nounPhraseSP "index")
  ("used to show a quantity applies to only one slice")) (const lI) Natural Nothing 

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
