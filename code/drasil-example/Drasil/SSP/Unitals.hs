module Drasil.SSP.Unitals where --export all of it

import Language.Drasil
import Language.Drasil.ShortHands

import Drasil.SSP.Defs (fsConcept)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, m_3, newton, pascal, specificWeight)

import Data.Drasil.Units.Physics (forcePerMeterU, momentOfForceU)

import Data.Drasil.Quantities.Math (area, pi_, unitVectj)
import Data.Drasil.Quantities.PhysicalProperties (density, mass, specWeight, vol)
import Data.Drasil.Quantities.Physics (acceleration, displacement, force, 
  gravitationalAccel, height, pressure, subX, subY, subZ, supMax, supMin,
  torque, weight)


symbols :: [DefinedQuantityDict]
symbols = map dqdWr inputs ++ map dqdWr outputs ++ 
  map dqdWr units ++ map dqdWr unitless

---------------------------
-- Imported UnitalChunks --
---------------------------
{-
SM.mobShear, SM.shearRes <- currently not used
SM.poissnsR, SM.elastMod <- Used to make UncertQ
-}
genericF = force
genericA = area

-- FIXME: These need to be imported here because they are used in generic TMs/GDs that SSP also imports. Automate this?
genericV = vol
genericW = weight
genericSpWght = specWeight
accel = acceleration
genericMass = mass
gravAccel = gravitationalAccel
dens = density
genericH = height
genericP = pressure
genericR = displacement
genericT = torque

-------------
-- HELPERS --
-------------
wiif :: String
wiif  = "without the influence of interslice forces"

--------------------------------
-- START OF CONSTRAINEDCHUNKS --
--------------------------------

constrained :: [ConstrainedChunk]
constrained = map cnstrw inputsWUncrtn ++ map cnstrw outputs

inputsWUncrtn :: [UncertQ]
inputsWUncrtn = [slopeDist, slopeHght, waterDist, waterHght, xMaxExtSlip, 
  xMaxEtrSlip, xMinExtSlip, xMinEtrSlip, yMaxSlip, yMinSlip, effCohesion, 
  fricAngle, dryWeight, satWeight, waterWeight]

inputsNoUncrtn :: [DefinedQuantityDict]
inputsNoUncrtn = [constF]

inputs :: [DefinedQuantityDict]
inputs = map dqdWr inputsWUncrtn ++ map dqdWr inputsNoUncrtn

outputs :: [ConstrConcept]
outputs = [fs, coords]

{-
monotonicIn :: [Constraint]  --FIXME: Move this?
monotonicIn = [physc $ \_ -> -- FIXME: Hack with "index" !
  (idx xi (sy index) $< idx xi (sy index + 1) $=> idx yi (sy index) $< idx yi (sy index + 1))]
-}

slopeDist, slopeHght, waterDist, waterHght, xMaxExtSlip, xMaxEtrSlip, 
  xMinExtSlip, xMinEtrSlip, yMaxSlip, yMinSlip, effCohesion, fricAngle, 
  dryWeight, satWeight, waterWeight :: UncertQ

constF :: DefinedQuantityDict
  
fs, coords :: ConstrConcept

{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals
--FIXME: add constraints to coordinate unitals when that is possible (constraints currently in the Notes section of the crtSlpId IM instead)

slopeDist = uqc "x_slope,i" (cn "x-coordinates of the slope")
  "x-coordinates of points on the soil slope"
  (sub (vec lX) lSlope) metre Real [] (dbl 0) defaultUncrt

slopeHght = uqc "y_slope,i" (cn "y-coordinates of the slope")
  "y-coordinates of points on the soil slope"
  (sub (vec lY) lSlope) metre Real [] (dbl 0) defaultUncrt

waterDist = uqc "x_wt,i" (cn "x-coordinates of the water table")
  "x-positions of the water table"
  (sub (vec lX) lWatTab) metre Real [] (dbl 0) defaultUncrt

waterHght = uqc "y_wt,i" (cn "y-coordinates of the water table")
  "heights of the water table"
  (sub (vec lY) lWatTab) metre Real [] (dbl 0) defaultUncrt

xMaxExtSlip = uqc "x_slip^maxExt" (cn "maximum exit x-coordinate")
  "maximum potential x-coordinate for the exit point of a slip surface"
  (sup (sub lX lSlip) lMaxExt) metre Real [] (dbl 100) 
  defaultUncrt

xMaxEtrSlip = uqc "x_slip^maxEtr" (cn "maximum entry x-coordinate")
  "maximum potential x-coordinate for the entry point of a slip surface"
  (sup (sub lX lSlip) lMaxEtr) metre Real [] (dbl 20)
  defaultUncrt
  
xMinExtSlip = uqc "x_slip^minExt" (cn "minimum exit x-coordinate")
  "minimum potential x-coordinate for the exit point of a slip surface"
  (sup (sub lX lSlip) lMinExt) metre Real [] (dbl 50) 
  defaultUncrt

xMinEtrSlip = uqc "x_slip^minEtr" (cn "minimum exit x-coordinate")
  "minimum potential x-coordinate for the entry point of a slip surface"
  (sup (sub lX lSlip) lMinEtr) metre Real [] (dbl 0) 
  defaultUncrt

yMaxSlip = uqc "y_slip^max" (cn "maximum y-coordinate") 
  "maximum potential y-coordinate of a point on a slip surface"
  (supMax (sub lY lSlip)) metre Real [] (dbl 30) 
  defaultUncrt

yMinSlip = uqc "y_slip^min" (cn "minimum y-coordinate") 
  "minimum potential y-coordinate of a point on a slip surface"
  (supMin (sub lY lSlip)) metre Real [] (dbl 0) 
  defaultUncrt

effCohesion = uqc "c'" (cn "effective cohesion")
  "internal pressure that sticks particles of soil together"
  (prime $ Variable "c") pascal Real [gtZeroConstr] (dbl 10000) defaultUncrt

fricAngle = uqc "varphi'" (cn "effective angle of friction")
  ("The angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime vPhi) degree Real [physc $ Bounded (Exc,0) (Exc,90)]
  (dbl 25) defaultUncrt

dryWeight = uqc "gamma" (cn "soil dry unit weight")
  "The weight of a dry soil/ground layer divided by the volume of the layer."
  (sub lGamma lDry) specificWeight Real [gtZeroConstr]
  (dbl 20000) defaultUncrt

satWeight = uqc "gamma_sat" (cn "soil saturated unit weight")
  ("The weight of saturated soil/ground " ++
  "layer divided by the volume of the layer.")
  (sub lGamma lSat) specificWeight Real [gtZeroConstr]
  (dbl 20000) defaultUncrt

waterWeight = uqc "gamma_w" (cn "unit weight of water")
  "The weight of one cubic meter of water."
  (sub lGamma lW) specificWeight Real [gtZeroConstr]
  (dbl 9800) defaultUncrt

constF = dqd' (dcc "const_f" (nounPhraseSP "decision on f") 
  ("boolean decision on which form of f the user desires: constant if true," ++
  " or half-sine if false")) (const (Variable "const_f")) Boolean Nothing

{-Output Variables-} --FIXME: See if there should be typical values
fs = constrained' (dqd' fsConcept (const $ sub cF lSafety) Real Nothing)
  [gtZeroConstr] (dbl 1)

fsMin :: DefinedQuantityDict -- This is a hack to remove the use of indexing for 'min'.
fsMin = dqd' (dcc "fsMin" (cn "minimum factor of safety") 
  "The minimum factor of safety associated with the critical slip surface")
  (const $ supMin (eqSymb fs)) Real Nothing 
-- Once things are converted to the new style of instance models, this will
-- be removed/fixed.

coords = cuc' "(x,y)"
  (cn "cartesian position coordinates" )
  ("y is considered parallel to the direction of the force of " ++
  "gravity and x is considered perpendicular to y")
  lCoords metre Real [] (dbl 1)

---------------------------
-- START OF UNITALCHUNKS --
---------------------------

units :: [UnitaryConceptDict]
units = map ucw [accel, genericMass, genericF, genericA, genericV, genericW,
  genericSpWght, gravAccel, dens, genericH, genericP, genericR, genericT, 
  nrmShearNum, nrmShearDen, slipHght, xi, yi, zcoord, critCoords, slipDist, 
  mobilizedShear, resistiveShear, mobShrI, shrResI, shearFNoIntsl, 
  shearRNoIntsl, slcWght, watrForce, intShrForce, baseHydroForce, 
  surfHydroForce, totNrmForce, nrmFSubWat, surfLoad, baseAngle, surfAngle, 
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, momntOfBdy, 
  porePressure, sliceHght, sliceHghtW, fx, fy, nrmForceSum, watForceSum, 
  sliceHghtRight, sliceHghtLeft, intNormForce, shrStress, totStress, 
  effectiveStress, effNormStress, dryVol, satVol, rotForce, momntArm]

accel, genericMass, genericF, genericA, genericV, genericW, genericSpWght, 
  gravAccel, dens, genericH, genericP, genericR, genericT, nrmShearNum, 
  nrmShearDen, slipDist, slipHght, xi, yi, zcoord, critCoords, mobilizedShear,
  mobShrI, sliceHght, sliceHghtW, shearFNoIntsl, shearRNoIntsl, slcWght, 
  watrForce, resistiveShear, shrResI, intShrForce, baseHydroForce, 
  surfHydroForce, totNrmForce, nrmFSubWat, surfLoad, baseAngle, surfAngle, 
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, momntOfBdy, fx, fy, 
  nrmForceSum, watForceSum, sliceHghtRight, sliceHghtLeft, porePressure, 
  intNormForce, shrStress, totStress, effectiveStress, effNormStress, dryVol, 
  satVol, rotForce, momntArm :: UnitalChunk
  
{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = uc' "G_i" (cn "interslice normal forces")
  "per meter in the z-direction exerted between each pair of adjacent slices"
  (vec cG) forcePerMeterU

slipHght = uc' "y_slip,i" (cn "y-coordinates of the slip surface")
  "heights of the slip surface"
  (sub (vec lY) lSlip) metre

slipDist = uc' "x_slip,i" (cn "x-coordinates of the slip surface")
  "x-coordinates of points on the slip surface"
  (sub (vec lX) lSlip) metre

yi = uc' "y_i" (cn "y-coordinate") "in the Cartesian coordinate system" lY metre
  
xi = uc' "x_i" (cn "x-coordinate") "in the Cartesian coordinate system" lX metre

zcoord = uc' "z" (cn "z-coordinate") "in the Cartesian coordinate system" lZ metre

-- FIXME: the 'symbol' for this should not have { and } embedded in it.
-- They have been removed now, but we need a reasonable notation.
critCoords = uc' "(xcs,ycs)" (cn "the set of x and y coordinates")
  "describe the vertices of the critical slip surface"
  (Concat [sub (vec lX) lCSlip, Label ",", sub (vec lY) lCSlip]) metre

mobilizedShear = uc' "mobilizedShear" (cn "mobilized shear force")
  "shear force in the direction of potential motion" cS newton

resistiveShear = uc' "resistiveShear" (cn "resistive shear force")
  ("Mohr Coulomb frictional force that describes the limit of mobilized " ++
  "shear force that can be withstood before failure") cP newton

mobShrI = uc' "mobShr" (cn "mobilized shear forces")
  "per meter in the z-direction for each slice"
  (vec cS) forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.mobShear
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shrResI = uc' "shrRes" (cn "resistive shear forces") ("Mohr Coulomb " ++
  "frictional forces per meter in the z-direction for each slice that " ++
  "describes the limit of mobilized shear force the slice can withstand " ++
  "before failure")
  (vec cP) forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.shearRes
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shearFNoIntsl = uc' "T_i"
  (cn ("mobilized shear forces " ++ wiif)) 
  "per meter in the z-direction for each slice"
  (vec cT) forcePerMeterU

shearRNoIntsl = uc' "R_i"
  (cn ("resistive shear forces " ++ wiif))
  "per meter in the z-direction for each slice"
  (vec cR) forcePerMeterU

slcWght = uc' "W_i" (cn "weights")
  "downward force per meter in the z-direction on each slice caused by gravity"
  (vec cW) forcePerMeterU

watrForce = uc' "H_i" (cn "interslice normal water forces") 
  ("per meter in the z-direction exerted in the x-coordinate direction " ++
  "between each pair of adjacent slices")
  (vec cH) forcePerMeterU

intShrForce = uc' "X_i" (cn "interslice shear forces") 
  "per meter in the z-direction exerted between adjacent slices"
  (vec cX) forcePerMeterU

baseHydroForce = uc' "U_b,i" (cn "base hydrostatic forces")
  "per meter in the z-direction from water pressure within each slice"
  (sub (vec cU) lBase) forcePerMeterU

surfHydroForce = uc' "U_t,i" (cn "surface hydrostatic forces")
  ("per meter in the z-direction from water pressure acting into each slice " ++
   "from standing water on the slope surface")
  (sub (vec cU) lSurface) forcePerMeterU

totNrmForce = uc' "N_i" (cn "normal forces")
  ("total reactive forces per meter in the z-direction for each slice of a " ++
  "soil surface subject to a body resting on it")
  (vec cN) forcePerMeterU

nrmFSubWat = uc' "N'_i" (cn "effective normal forces")
  ("per meter in the z-direction for each slice of a soil surface, " ++
  "subtracting pore water reactive force from total reactive force") 
  (vec (prime $ Variable "N")) forcePerMeterU

surfLoad = uc' "Q_i" (cn "external forces") 
  "forces per meter in the z-direction acting into the surface from the midpoint of each slice"
  (vec cQ) forcePerMeterU

baseAngle = uc' "alpha_i" (cn "base angles")
  "between the base of each slice and the horizontal"
  (vec lAlpha) degree

surfAngle = uc' "beta_i" (cn "surface angles")
  "between the surface of each slice and the horizontal"
  (vec lBeta) degree

impLoadAngle = uc' "omega_i" (cn "imposed load angles")
  ("between the external force acting into the surface of each slice and the" ++
  " vertical")
  (vec lOmega) degree

baseWthX = uc' "b_i" (cn "base width of slices")
  "in the x-direction"
  (vec lB) metre

baseLngth = uc' "l_b,i" (cn "total base lengths of slices") 
  "in the direction parallel to the slope of the base of each slice"
  (sub (vec lEll) lB) metre

surfLngth = uc' "l_s,i" (cn "surface lengths of slices")
  "in the direction parallel to the slope of the surface of each slice"
  (sub (vec lEll) lS) metre

midpntHght = uc' "h_i" (cn "y-direction heights of slices")
  ("heights in the y-direction from the base of each slice to the slope " ++
  "surface, at the x-direction midpoint of the slice")
  (vec lH) metre

momntOfBdy = uc' "M" (cn' "moment") ("a measure of the tendency of " ++
  "a body to rotate about a specific point or axis")
  cM momentOfForceU --FIXME: move in concepts.physics ?

porePressure = uc' "u" (cn "pore pressure") "from water within the soil"
  lU pascal
  
shrStress = uc' "tau_i" (cn "shear strength") "" lTau pascal

sliceHght = uc' "h_z,i" (cn "heights of interslice normal forces")
  "the heights in the y-direction of the interslice normal forces on each slice"
  (subZ (vec lH)) metre

sliceHghtW = uc' "h_z,w,i" (cn "heights of the water table")
  ("the heights in the y-direction from the base of each slice to the water" ++
  "table") (sub (vec lH) lHeights) metre

nrmShearNum = uc' "C_num,i" (cn "proportionality constant numerator")
  ("values for each slice that sum together to form the numerator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lNum) newton
  
nrmShearDen = uc' "C_den,i" (cn "proportionality constant denominator")
  ("values for each slice that sum together to form the denominator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lDen) newton

fx = uc' "fx" (cn "x-coordinate of the force") "" (subX cF) newton

fy = uc' "fy" (cn "y-coordinate of the force") "" (subY cF) newton

nrmForceSum = uc' "F_x^G" (cn "sums of the interslice normal forces") 
  "for each pair of adjacent interslice boundaries"
  (sup (subX (vec cF)) lNorm) newton

watForceSum = uc' "F_x^H" (cn "sums of the interslice normal water forces") 
  "for each pair of adjacent interslice boundaries"
  (sup (subX (vec cF)) lNormWat) newton

sliceHghtRight = uc' "h^R" (cn "heights of the right side of slices") 
  "assuming slice surfaces have negative slope"
  (sup (vec lH) lRight) metre

sliceHghtLeft = uc' "h^L" (cn "heights of the left side of slices") 
  "assuming slice surfaces have negative slope"
  (sup (vec lH) lLeft) metre

totStress = uc' "sigma" (cn' "total stress")
  "on the soil mass" lSigma pascal

effectiveStress = uc' "sigma'" (cn' "effective stress")
  "provided by the soil skeleton" (prime lSigma) pascal

effNormStress = uc' "sigmaN'" (cn' "effective normal stress")
  "" (prime $ sub lSigma cN) pascal

dryVol = uc' "V_dry" (cn "volumes of dry soil")
  "amount of space occupied by dry soil for each slice"
  (sub (vec cV) lDry) m_3

satVol = uc' "V_sat" (cn "volumes of saturated soil")
  "amount of space occupied by saturated soil for each slice"
  (sub (vec cV) lSat) m_3

rotForce = uc' "F_rot" (cn "force causing rotation") 
  "a force in the direction of rotation" (sub cF lRot) newton
  
momntArm = uc' "r" (cn' "length of the moment arm") 
  "distance between a force causing rotation and the axis of rotation"
  lR metre

----------------------
-- Unitless Symbols --
----------------------

unitless :: [DefinedQuantityDict]
unitless = [earthqkLoadFctr, normToShear, scalFunc, numbSlices, minFunction, 
  mobShrC, shrResC, index, pi_, varblV, fsMin, unitVectj]

earthqkLoadFctr, normToShear, scalFunc, numbSlices,
  minFunction, mobShrC, shrResC, index, varblV :: DefinedQuantityDict

earthqkLoadFctr = dqd' (dcc "K_c" (nounPhraseSP "seismic coefficient")
  ("proportionality factor of force that " ++
  "weight pushes outwards; caused by seismic earth movements"))
  (const $ sub cK lCoeff) Real Nothing 

normToShear = dqd' (dcc "lambda"
  (nounPhraseSP "proportionality constant")
  "for the interslice normal to shear force ratio") (const lLambda) Real Nothing

scalFunc = dqd' (dcc "f_i" (nounPhraseSP $ "interslice normal to shear " ++
  "force ratio variation function")
  "function of distance in the x-direction")
  (const (vec lF)) Real Nothing 

numbSlices = dqd' (dcc "n" (nounPhraseSP "number of slices")
  "the slip mass has been divided into")
  (const lN) Natural Nothing

minFunction = dqd' (dcc "Upsilon" (nounPhraseSP "minimization function")
  "generic minimization function or algorithm")
  (const cUpsilon) Real Nothing

mobShrC = dqd' (dcc "Psi" (nounPhraseSP $ "second function for incorporating" ++
  " interslice forces into shear force") ("converts mobile shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces"))
  (const (vec cPsi)) Real Nothing

shrResC = dqd' (dcc "Phi" (nounPhraseSP $ "first function for incorporating " ++
  "interslice forces into shear force") ("converts resistive shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces"))
  (const (vec cPhi)) Real Nothing

--------------------
-- Index Function --
--------------------

varblV = dqd' (dcc "varblV" (nounPhraseSP "local index")
  "used as a bound variable index in calculations")
  (const lV) Natural Nothing

index = dqd' (dcc "index" (nounPhraseSP "index")
  "representing a single slice") (const lI) Natural Nothing 

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

-- Labels

lBase, lCoeff, lCoords, lCSlip, lDen, lDry, lHeights, lLeft, lMaxEtr, lMaxExt,
  lMinEtr, lMinExt, lNorm, lNormWat, lNum, lRight, lRot, lSafety, lSat, lSlip,
  lSlope, lSurface, lWatTab :: Symbol
lBase    = Label "b"
lCoeff   = Label "c"
lCoords  = Label "(x,y)"
lCSlip   = Label "cs"
lDen     = Label "den"
lDry     = Label "dry"
lHeights = Label "z,w"
lLeft    = Label "L"
lMaxEtr  = Label "maxEtr"
lMaxExt  = Label "maxExt"
lMinEtr  = Label "minEtr"
lMinExt  = Label "minExt"
lNorm    = Label "G"
lNormWat = Label "H"
lNum     = Label "num"
lRight   = Label "R"
lRot     = Label "rot"
lSafety  = Label "S"
lSat     = Label "sat"
lSlip    = Label "slip"
lSlope   = Label "slope"
lSurface = Label "g"
lWatTab  = Label "wt"
