module Drasil.SSP.Unitals where --export all of it

import Language.Drasil
import Language.Drasil.ShortHands

import Drasil.SSP.Defs (fs_concept)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, newton, pascal, specific_weight)

import Data.Drasil.Units.Physics (forcePerMeterU, momentOfForceU)

import Data.Drasil.Quantities.Math (area, pi_)
import Data.Drasil.Quantities.Physics (force)


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
sspInputs = [effCohesion, fricAngle, dryWeight, satWeight, waterWeight]

sspOutputs :: [ConstrConcept]
sspOutputs = [fs, coords]

{-
monotonicIn :: [Constraint]  --FIXME: Move this?
monotonicIn = [physc $ \_ -> -- FIXME: Hack with "index" !
  (idx xi (sy index) $< idx xi (sy index + 1) $=> idx yi (sy index) $< idx yi (sy index + 1))]
-}

defultUncrt :: Double
defultUncrt = 0.1

effCohesion, fricAngle, dryWeight, satWeight,
  waterWeight :: UncertQ
  
fs, coords :: ConstrConcept

{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals

effCohesion = uqc "c'" (cn $ "effective cohesion")
  "internal pressure that sticks particles of soil together"
  (prime $ Atomic "c") pascal Real [gtZeroConstr] (dbl 10) defultUncrt

fricAngle = uqc "varphi'" (cn $ "effective angle of friction")
  ("The angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime $ vPhi) degree Real [physc $ Bounded (Exc,0) (Exc,90)]
  (dbl 25) defultUncrt

dryWeight = uqc "gamma" (cn $ "soil dry unit weight")
  "The weight of a dry soil/ground layer divided by the volume of the layer."
  lGamma specific_weight Real [gtZeroConstr]
  (dbl 20) defultUncrt

satWeight = uqc "gamma_sat" (cn $ "soil saturated unit weight")
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
sspUnits = map ucw [genericF, genericA, nrmShearNum, nrmShearDen, waterHght, 
  slopeHght, slipHght, xi, yi, zcoord, critCoords, waterDist, slopeDist, slipDist,
  mobShrI, shrResI, shearFNoIntsl, shearRNoIntsl, slcWght, slcWghtR, slcWghtL,
  watrForce, intShrForce, baseHydroForce, baseHydroForceR, 
  baseHydroForceL, surfHydroForce, surfHydroForceR, surfHydroForceL, 
  totNrmForce, nrmFSubWat, nrmFNoIntsl, surfLoad, baseAngle, surfAngle, 
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, momntOfBdy, 
  porePressure, sliceHght, sliceHghtW, fx, fy, nrmForceSum, watForceSum, 
  sliceHghtRight, sliceHghtLeft, intNormForce, shrStress, 
  totStress, effectiveStress, effNormStress]

genericF, genericA, nrmShearNum, nrmShearDen, waterDist, slopeDist, slipDist, waterHght, 
  slopeHght, slipHght, xi, yi, zcoord, critCoords, mobShrI, sliceHght,
  sliceHghtW, shearFNoIntsl, shearRNoIntsl, slcWght, slcWghtR, slcWghtL, 
  watrForce, shrResI, intShrForce, baseHydroForce, baseHydroForceR, 
  baseHydroForceL, surfHydroForce,surfHydroForceR, surfHydroForceL, totNrmForce,
  nrmFSubWat, nrmFNoIntsl, surfLoad, baseAngle, surfAngle, impLoadAngle, 
  baseWthX, baseLngth, surfLngth, midpntHght, momntOfBdy, fx, fy, nrmForceSum, 
  watForceSum, sliceHghtRight, sliceHghtLeft, porePressure,
  intNormForce, shrStress, totStress, effectiveStress, 
  effNormStress :: UnitalChunk
  
{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = uc' "G_i" (cn $ "interslice normal force")
  ("per meter in the z-direction exerted between adjacent slices")
  (cG) forcePerMeterU

waterHght = uc' "y_wt,i"
  (cn $ "y-coordinate of the water table")
  ("height of the water table")
  (sub lY (Atomic "wt")) metre

slopeHght = uc' "y_slope,i" (cn $ "y-coordinate of the slope")
  ("y-coordinate of a point on the soil slope")
  (sub lY (Atomic "slope")) metre

slipHght = uc' "y_slip,i" (cn $ "y-coordinate of the slip surface")
  ("height of the slip surface")
  (sub lY (Atomic "slip")) metre

waterDist = uc' "x_wt,i"
  (cn $ "x-coordinate")
  ("x-position of the water table")
  (sub lX (Atomic "wt")) metre

slopeDist = uc' "x_slope,i" (cn $ "x-coordinate of the slope")
  ("x-coordinate of a point on the slope")
  (sub lX (Atomic "slope")) metre

slipDist = uc' "x_slip,i" (cn $ "x-coordinate of the slip surface")
  ("distance of the slip surface")
  (sub lX (Atomic "slip")) metre

yi = uc' "y_i" (cn $ "y-coordinate") "in the Cartesian coordinate system" lY metre
  
xi = uc' "x_i" (cn $ "x-coordinate") "in the Cartesian coordinate system" lX metre

zcoord = uc' "z" (cn $ "z-coordinate") "in the Cartesian coordinate system" lZ metre

-- FIXME: the 'symbol' for this should not have { and } embedded in it.
-- They have been removed now, but we need a reasonable notation.
critCoords = uc' "(xcs,ycs)" (cn $ "the set of x and y coordinates")
  "describe the vertices of the critical slip surface"
  (Concat [sub (Atomic "x") (Atomic "cs"), Atomic ",",
  sub (Atomic "y") (Atomic "cs")]) metre

mobShrI = uc' "mobShr" (cn $ "mobilized shear force")
  "per meter in the z-direction"
  (cS) forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.mobShear
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shrResI = uc' "shrRes" (cn $ "resistive shear force") ("Mohr Coulomb " ++
  "frictional force per meter in the z-direction that describes the limit of" ++
  " mobilized shear force a slice can withstand before failure")
  (cP) forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.shearRes
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shearFNoIntsl = uc' "T_i"
  (cn $ ("mobilized shear force " ++ wiif)) 
  "per meter in the z-direction"
  cT forcePerMeterU

shearRNoIntsl = uc' "R_i"
  (cn $ ("resistive shear force " ++ wiif))
  "per meter in the z-direction"
  (cR) forcePerMeterU

slcWght = uc' "W_i" (cn $ "weight")
  ("downward force per meter in the z-direction caused by gravity on slice i")
  (cW) forcePerMeterU
  
slcWghtR = uc' "W^R" (cn $ "right weight of a slice") 
  ("weight of a slice per meter in the z-direction, assuming the entire " ++ "slice has the height of the right side of the slice") 
  (sup cW cR) forcePerMeterU

slcWghtL = uc' "W^L" (cn $ "left weight of a slice") 
  ("weight of a slice per meter in the z-direction, assuming the entire " ++
  "slice has the height of the left side of the slice") 
  (sup cW cL) forcePerMeterU

watrForce = uc' "H_i" (cn $ "interslice normal water force ") 
  ("per meter in the z-direction exerted in the x-ordinate direction between" ++
  " adjacent slices")
  (cH) forcePerMeterU

intShrForce = uc' "X_i" (cn $ "interslice shear force") 
  ("per meter in the z-direction exerted between adjacent slices")
  (cX) forcePerMeterU

baseHydroForce = uc' "U_b,i" (cn $ "base hydrostatic force")
  ("per meter in the z-direction from water pressure within a slice")
  (sub cU lB) forcePerMeterU

baseHydroForceR = uc' "U^R_b,i" (cn $ "right base hydrostatic force on a slice")
  ("per meter in the z-direction from water pressure within a slice, " ++
  "assuming the entire slice has the height of the right side of the slice")
  (sub (sup cU cR) lB) forcePerMeterU

baseHydroForceL = uc' "U^L_b,i" (cn $ "left base hydrostatic force on a slice")
  ("per meter in the z-direction from water pressure within a slice, " ++
  "assuming the entire slice has the height of the left side of the slice")
  (sub (sup cU cL) lB) forcePerMeterU

surfHydroForce = uc' "U_t,i" (cn $ "surface hydrostatic force")
  ("per meter in the z-direction from water pressure acting into the slice " ++
   "from standing water on the slope surface")
  (sub cU lT) forcePerMeterU

surfHydroForceR = uc' "U^R_t,i" (cn $ "right surface hydrostatic force on a slice")
  ("per meter in the z-direction from water pressure acting into the slice " ++ "from standing water on the slope surface, assuming the entire slice has " ++
  "the height of the right side of the slice")
  (sub (sup cU cR) lT) forcePerMeterU

surfHydroForceL = uc' "U^L_t,i" (cn $ "left surface hydrostatic force on a slice")
  ("per meter in the z-direction from water pressure acting into the slice " ++
  "from standing water on the slope surface, assuming the entire slice has " ++
  "the height of the left side of the slice")
  (sub (sup cU cL) lT) forcePerMeterU

totNrmForce = uc' "N_i" (cn $ "normal force")
  ("total reactive force per meter in the z-direction for a soil surface subject to a body resting on it")
  cN forcePerMeterU

nrmFSubWat = uc' "N'_i" (cn $ "effective normal force")
  ("per meter in the z-direction for a soil surface, subtracting pore water reactive force from total " ++
  "reactive force") (prime $ Atomic "N") forcePerMeterU

nrmFNoIntsl = uc' "N*_i" (cn $ "effective normal force")
  ("for a soil surface, " ++ wiif) (Atomic "N*") newton

surfLoad = uc' "Q_i" (cn' $ "external force") 
  "a force per meter in the z-direction acting into the surface from the midpoint of a slice"
  (cQ) forcePerMeterU

baseAngle = uc' "alpha_i" (cn $ "base angle")
  ("between the base of a slice and the horizontal")
  lAlpha degree

surfAngle = uc' "beta_i" (cn $ "surface angle")
  ("between the surface of a slice and the horizontal")
  lBeta degree

impLoadAngle = uc' "omega_i" (cn $ "imposed load angle")
  ("between the external force acting into the surface and the vertical")
  lOmega degree

baseWthX = uc' "b_i" (cn $ "base width of a slice")
  ("in the x-direction")
  (lB) metre

baseLngth = uc' "l_b,i" (cn $ "total base length of a slice") 
  "in the direction parallel to the slope of the base"
  (sub lEll lB) metre

surfLngth = uc' "l_s,i" (cn $ "surface length of a slice")
  "in the direction parallel to the slope of the surface"
  (sub lEll lS) metre

midpntHght = uc' "h_i" (cn $ "y-direction height of a slice")
  ("height in the y-direction from the base of a slice to the slope " ++
  "surface, at the x-direction midpoint of the slice")
  (lH) metre

momntOfBdy = uc' "M" (cn' $ "net moment") ("a measure of the tendency of " ++
  "a body to rotate about a specific point or axis")
  cM momentOfForceU --FIXME: move in concepts.physics ?

porePressure = uc' "u" (cn "pore pressure") ("from water within the soil")
  lU pascal
  
shrStress = uc' "tau_i" (cn "shear strength") "" lTau pascal

sliceHght = uc' "h_z,i" (cn "height of center of slice")
  ("the height in the y-direction from the base of a slice to the " ++
  "center of the slice")
  (sub lH lZ) metre

sliceHghtW = uc' "h_z,w,i" (cn "height halfway to water table")
  ("the height in the y-direction from the base of a slice halfway to the " ++
  "water table")
  (sub lH (Atomic "z,w")) metre

nrmShearNum = uc' "C_num,i" (cn "proportionality constant numerator")
  ("expression used to calculate the numerator of the interslice normal to " ++
  "shear force proportionality constant")
  (sub cC (Atomic "num")) newton
  
nrmShearDen = uc' "C_den,i" (cn "proportionality constant denominator")
  ("expression used to calculate the denominator of the interslice normal to" ++
  " shear force proportionality constant")
  (sub cC (Atomic "den")) newton

fx = uc' "fx" (cn "x-component of the net force") ""
  (sub cF lX) newton

fy = uc' "fy" (cn "y-component of the net force") ""
  (sub cF lY) newton

nrmForceSum = uc' "F_x^G" (cn "sum of the interslice normal forces") 
  "for two adjacent interslice boundaries"
  (sup (sub cF lX) cG) newton

watForceSum = uc' "F_x^H" (cn "sum of the interslice normal water forces") 
  "for two adjacent interslice boundaries"
  (sup (sub cF lX) cH) newton

sliceHghtRight = uc' "h^R" (cn "height of the right side of a slice") 
  "assuming slice surface has negative slope"
  (sup lH cR) metre

sliceHghtLeft = uc' "h^L" (cn "height of the left side of a slice") 
  "assuming slice surface has negative slope"
  (sup lH cL) metre

totStress = uc' "sigma" (cn' $ "total stress") "on the soil mass" lSigma pascal

effectiveStress = uc' "sigma'" (cn' $ "effective stress") "provided by the soil skeleton" (prime lSigma) pascal

effNormStress = uc' "sigmaN'" (cn' "effective normal stress") "" (prime $ sub lSigma cN) pascal
  
----------------------
-- Unitless Symbols --
----------------------

sspUnitless :: [DefinedQuantityDict]
sspUnitless = [constF, earthqkLoadFctr, normToShear, scalFunc,
  numbSlices, minFunction, mobShrC, shrResC, index, pi_, varblV, fs_min]

constF, earthqkLoadFctr, normToShear, scalFunc, numbSlices,
  minFunction, mobShrC, shrResC, index, varblV :: DefinedQuantityDict

constF = dqd' (dcc "const_f" (nounPhraseSP $ "decision on f") 
  ("boolean decision on which form of f the user desires: constant if true," ++
  " or half-sine if false")) (const (Atomic "const_f")) Boolean Nothing

earthqkLoadFctr = dqd' (dcc "K_c" (nounPhraseSP $ "seismic coefficient")
  ("proportionality factor of force that " ++
  "weight pushes outwards; caused by seismic earth movements"))
  (const $ sub cK lC) Real Nothing 

normToShear = dqd' (dcc "lambda"
  (nounPhraseSP $ "proportionality constant")
  ("for the interslice normal to shear force ratio")) (const lLambda) Real Nothing

scalFunc = dqd' (dcc "f_i" (nounPhraseSP $ "interslice normal to shear " ++
  "force ratio variation function")
  ("function of distance in the x-direction"))
  (const lF) Real Nothing 

numbSlices = dqd' (dcc "n" (nounPhraseSP "number of slices")
  "the slip mass has been divided into")
  (const lN) Natural Nothing

minFunction = dqd' (dcc "Upsilon" (nounPhraseSP "function")
  ("generic minimization function or algorithm"))
  (const cUpsilon) Real Nothing

mobShrC = dqd' (dcc "Psi" (nounPhraseSP $ "second function for incorporating" ++
  " interslice forces into shear force") ("converts mobile shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces"))
  (const cPsi) Real Nothing

shrResC = dqd' (dcc "Phi" (nounPhraseSP $ "first function for incorporating " ++
  "interslice forces into shear force") ("converts resistive shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces"))
  (const cPhi) Real Nothing

--------------------
-- Index Function --
--------------------

varblV = dqd' (dcc "varblV" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  (const lV) Natural Nothing

index = dqd' (dcc "index" (nounPhraseSP "index")
  ("representing a single slice")) (const lI) Natural Nothing 

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
