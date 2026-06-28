module Drasil.SSP.Unitals (module Drasil.SSP.Unitals) where --export all of it

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.Database (mkUid)
import Drasil.SSP.Defs (fsConcept,slpSrf,minim,maxim,waterTable,slope,xCoords,yCoords)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, m_3, newton, pascal, specificWeight)

import Data.Drasil.Units.Physics (forcePerMeterU)

import Data.Drasil.Concepts.Math (cartesian, xCoord, xDir, yCoord, yDir,
  zCoord, zDir)
import Data.Drasil.Concepts.Physics (gravity)

import Data.Drasil.Quantities.Math (area, pi_, unitVectj, surface)
import Data.Drasil.Quantities.PhysicalProperties (density, mass, specWeight,
  vol, len)
import Data.Drasil.Quantities.Physics (acceleration, displacement, distance,
  force, gravitationalAccel, height, moment2D, pressure, subX, subY, subZ,
  supMax, supMin, torque, weight, positionVec, time)

symbols :: [DefinedQuantityDict]
symbols = dqdWr coords : NE.toList inputs ++ map dqdWr (NE.toList outputs)
  ++ units ++ unitless

---------------------------
-- Imported UnitalChunks --
---------------------------
{-
SM.mobShear, SM.shearRes <- currently not used
SM.poissnsR, SM.elastMod <- Used to make UncertQ
-}
genericF = force
genericA = area
genericM = moment2D

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
posVec = positionVec

-------------
-- HELPERS --
-------------
wiif :: String
wiif = "without the influence of interslice forces"

--------------------------------
-- START OF CONSTRAINEDCHUNKS --
--------------------------------

constrained :: [ConstrConcept]
constrained = coords : map cnstrw' (NE.toList inputsWUncrtn) ++ NE.toList outputs

inputsWUncrtn :: NE.NonEmpty UncertQ
inputsWUncrtn = slopeDist :| [slopeHght, waterDist, waterHght, xMaxExtSlip,
  xMaxEtrSlip, xMinExtSlip, xMinEtrSlip, yMaxSlip, yMinSlip, effCohesion,
  fricAngle, dryWeight, satWeight, waterWeight]

inputsNoUncrtn :: NE.NonEmpty DefinedQuantityDict
inputsNoUncrtn = constF :| []

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = NE.map dqdWr inputsWUncrtn <> inputsNoUncrtn

outputs :: NE.NonEmpty ConstrConcept
outputs = NE.singleton fs

{-
monotonicIn :: [Constraint]  --FIXME: Move this?
monotonicIn = [physRange $ \_ -> -- FIXME: Hack with "index" !
  (idx xi (sy index) $< idx xi (sy index + 1) $=> idx yi (sy index) $< idx yi (sy index + 1))]
-}

slopeDist, slopeHght, waterDist, waterHght, xMaxExtSlip, xMaxEtrSlip,
  xMinExtSlip, xMinEtrSlip, yMaxSlip, yMinSlip, effCohesion, fricAngle,
  dryWeight, satWeight, waterWeight :: UncertQ

{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals
--FIXME: add constraints to coordinate unitals when that is possible (constraints currently in the Notes section of the crtSlpId IM instead)

slopeDist = uq (constrained' (quant (mkUid "x_slope,i") ((xCoords ^. term) `NP.of_` the slope)
  (plural xCoord `S.of_` S "points on the soil slope")
  (sub (vec lX) lSlope) (Vect Real) metre) [] (exactDbl 0)) defaultUncrt

slopeHght = uq (constrained' (quant (mkUid "y_slope,i") ((yCoords ^. term) `NP.of_` the slope)
  (plural yCoord `S.of_` S "points on the soil slope")
  (sub (vec lY) lSlope) (Vect Real) metre) [] (exactDbl 0)) defaultUncrt

waterDist = uqc "x_wt,i" ((xCoords ^. term) `NP.of_` the waterTable)
  "x-positions of the water table"
  (sub (vec lX) lWatTab) metre (Vect Real) [] (exactDbl 0) defaultUncrt

waterHght = uqc "y_wt,i" ((yCoords ^. term) `NP.of_` the waterTable)
  "heights of the water table"
  (sub (vec lY) lWatTab) metre (Vect Real) [] (exactDbl 0) defaultUncrt

xMaxExtSlip = uq (constrained' (quant (mkUid "x_slip^maxExt") (compoundPhrase (cn "maximum exit") (xCoord ^. term))
  (S "the maximum potential" +:+ phrase xCoord `S.for` S "the exit point" `S.ofA` S "slip surface")
  (sup (sub lX lSlip) lMaxExt) Real metre) [] (exactDbl 100)) defaultUncrt

xMaxEtrSlip = uq (constrained' (quant (mkUid "x_slip^maxEtr")
  (compoundPhrase (cn "maximum entry") (xCoord ^. term))
  (S "the maximum potential" +:+ phrase xCoord `S.for` S "the entry point" `S.ofA` S "slip surface")
  (sup (sub lX lSlip) lMaxEtr) Real metre) [] (exactDbl 20)) defaultUncrt

xMinExtSlip = uq (constrained' (quant (mkUid "x_slip^minExt")
  (compoundPhrase (cn "minimum exit") (xCoord ^. term))
  (S "the minimum potential" +:+ phrase xCoord `S.for` S "the exit point" `S.ofA` S "slip surface")
  (sup (sub lX lSlip) lMinExt) Real metre) [] (exactDbl 50)) defaultUncrt

xMinEtrSlip = uq (constrained' (quant (mkUid "x_slip^minEtr") (compoundPhrase (cn "minimum entry") (xCoord ^. term))
  (S "the minimum potential" +:+ phrase xCoord `S.for` S "the entry point" `S.ofA` S "slip surface")
  (sup (sub lX lSlip) lMinEtr) Real metre) [] (exactDbl 0)) defaultUncrt

yMaxSlip = uq (constrained' (quant (mkUid "y_slip^max") (compoundNC maxim yCoord ^. term)
  (S "the maximum potential" +:+ phrase yCoord `S.of_` S "a point on a slip surface")
  (supMax (sub lY lSlip)) Real metre) [] (exactDbl 30)) defaultUncrt

yMinSlip = uq (constrained' (quant (mkUid "y_slip^min") (compoundNC minim yCoord ^. term)
  (S "the minimum potential" +:+ phrase yCoord `S.of_` S "a point on a slip surface")
  (supMin (sub lY lSlip)) Real metre) [] (exactDbl 0)) defaultUncrt

effCohesion = uqc "c'" (cn "effective cohesion")
  "the internal pressure that sticks particles of soil together"
  (prime $ variable "c") pascal Real [gtZeroConstr] (exactDbl 10000) defaultUncrt

fricAngle = uqc "varphi'" (cn "effective angle of friction")
  ("the angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime vPhi) degree Real [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 90)]
  (exactDbl 25) defaultUncrt

dryWeight = uqc "gamma" (cn "soil dry unit weight")
  "the weight of a dry soil/ground layer divided by the volume of the layer"
  (sub lGamma lDry) specificWeight Real [gtZeroConstr]
  (exactDbl 20000) defaultUncrt

satWeight = uqc "gamma_sat" (cn "soil saturated unit weight")
  "the weight of saturated soil/ground layer divided by the volume of the layer"
  (sub lGamma lSat) specificWeight Real [gtZeroConstr]
  (exactDbl 20000) defaultUncrt

waterWeight = uqc "gamma_w" (cn "unit weight of water")
  "the weight of one cubic meter of water"
  (sub lGamma lW) specificWeight Real [gtZeroConstr]
  (exactDbl 9800) defaultUncrt

constF :: DefinedQuantityDict
constF = quantNoUnit (mkUid "const_f") (nounPhraseSP "decision on f")
  (S ("a Boolean decision on which form of f the user desires: constant if true," ++
  " or half-sine if false")) (variable "const_f") Boolean

{-Output Variables-} --FIXME: See if there should be typical values
fs, coords :: ConstrConcept
fs = constrained' (dqd' fsConcept (const $ sub cF lSafety) Real Nothing)
  [gtZeroConstr] (exactDbl 1)

fsMin :: DefinedQuantityDict -- This is a hack to remove the use of indexing for 'min'.
fsMin = quantNoUnit (mkUid "fsMin") (cn "minimum factor of safety")
  (S "the minimum factor of safety associated with the critical slip surface")
  (supMin (eqSymb fs)) Real
-- Once things are converted to the new style of instance models, this will
-- be removed/fixed.

coords = constrainedNRV' (quant (mkUid "(x,y)") (cn "cartesian position coordinates")
  (P lY +:+ S "is considered parallel to the direction of the force of" +:+
   phrase gravity `S.and_` P lX +:+ S "is considered perpendicular to" +:+ P lY)
  lCoords Real metre) []

---------------------------
-- START OF UNITALCHUNKS --
---------------------------

units :: [DefinedQuantityDict]
units = [accel, genericMass, genericF, genericA, genericM, genericV,
  genericW, genericSpWght, gravAccel, dens, genericH, genericP, genericR,
  genericT, nrmShearNum, nrmShearDen, slipHght, xi, yi, zcoord, critCoords,
  slipDist, mobilizedShear, resistiveShear, mobShrI, shrResI, shearFNoIntsl,
  shearRNoIntsl, slcWght, watrForce, intShrForce, baseHydroForce,
  surfHydroForce, totNrmForce, nrmFSubWat, surfLoad, baseAngle, surfAngle,
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght,
  porePressure, sliceHght, sliceHghtW, fx, fy, fn, ft, nrmForceSum, watForceSum,
  sliceHghtRight, sliceHghtLeft, intNormForce, shrStress, totNormStress, tangStress,
  effectiveStress, effNormStress, dryVol, satVol, rotForce, momntArm, posVec,
  time, surface, len]

accel, genericMass, genericF, genericA, genericM, genericV, genericW,
  genericSpWght, gravAccel, dens, genericH, genericP, genericR, genericT,
  nrmShearNum, nrmShearDen, slipDist, slipHght, xi, yi, zcoord, critCoords,
  mobilizedShear, mobShrI, sliceHght, sliceHghtW, shearFNoIntsl, shearRNoIntsl,
  slcWght, watrForce, resistiveShear, shrResI, intShrForce, baseHydroForce,
  surfHydroForce, totNrmForce, nrmFSubWat, surfLoad, baseAngle, surfAngle,
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, fx, fy, fn, ft,
  nrmForceSum, watForceSum, sliceHghtRight, sliceHghtLeft, porePressure,
  intNormForce, shrStress, totNormStress, tangStress, effectiveStress,
  effNormStress, dryVol, satVol, rotForce, momntArm, posVec :: DefinedQuantityDict

{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = quant (mkUid "G_i") (cn "interslice normal forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+
   S "exerted between each pair" `S.of_` S "adjacent slices")
  (vec cG) (Vect Real) forcePerMeterU

slipHght = quant (mkUid "y_slip,i") (yCoords `ofThe` slpSrf)
  (S "heights of the slip surface")
  (sub (vec lY) lSlip) (Vect Real) metre

slipDist = quant (mkUid "x_slip,i") (xCoords `ofThe` slpSrf)
  (plural xCoord `S.of_` S "points on the slip surface")
  (sub (vec lX) lSlip) (Vect Real) metre

xi = quant (mkUid "x_i") (xCoord ^. term)
  (D.toSent $ phraseNP (NP.the (xCoord `inThe` cartesian))) lX Real metre

yi = quant (mkUid "y_i") (yCoord ^. term)
  (D.toSent $ phraseNP (NP.the (yCoord `inThe` cartesian))) lY Real metre

zcoord = quant (mkUid "z") (zCoord ^. term)
  (D.toSent (phraseNP (NP.the (zCoord `inThe` cartesian)))) lZ Real metre

-- FIXME: the 'symbol' for this should not have { and } embedded in it.
-- They have been removed now, but we need a reasonable notation.
critCoords = quant (mkUid "(xcs,ycs)") (cn "critical slip surface coordinates")
  (S "the set" `S.of_` D.toSent (pluralNP (xCoord `and_PP` yCoord)) +:+
   S "that describe the vertices" `S.ofThe` S "critical slip surface")
  (Concat [sub (vec lX) lCSlip, label ",", sub (vec lY) lCSlip]) Real metre

mobilizedShear = quant (mkUid "mobilizedShear") (cn' "mobilized shear force")
  (S "the shear force" `S.inThe` S "direction" `S.of_` S "potential motion") cS Real newton

resistiveShear = quant (mkUid "resistiveShear") (cn' "resistive shear force")
  (S "the Mohr Coulomb frictional force that describes the limit" `S.of_`
    phrase mobilizedShear +:+ S "that can be withstood before failure")
  cP Real newton

mobShrI = quant (mkUid "mobShrFs") (cn' "mobilized shear force")
  (D.toSent (phraseNP (the mobilizedShear)) +:+ S "per meter" `S.inThe` phrase zDir +:+
   S "for each slice")
  (vec cS) Real forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.mobShear
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shrResI = quant (mkUid "shrRes") (cn "resistive shear forces")
  (S "the Mohr Coulomb frictional forces per meter" `S.inThe` phrase zDir +:+
   S "for each slice that describe the limit" `S.of_` phrase mobilizedShear +:+
   S "the slice can withstand before failure")
  (vec cP) Real forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.shearRes
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shearFNoIntsl = quant (mkUid "T_i") (cn ("mobilized shear forces " ++ wiif))
  (D.toSent (pluralNP (the mobilizedShear)) +:+ S "per meter" +:+ S wiif `S.inThe`
   phrase zDir `S.for` S "each slice")
  (vec cT) (Vect Real) forcePerMeterU

shearRNoIntsl = quant (mkUid "R_i") (cn ("resistive shear forces " ++ wiif))
  (D.toSent (pluralNP (the resistiveShear)) +:+ S "per meter" +:+ S wiif `S.inThe`
   phrase zDir `S.for` S "each slice")
  (vec cR) (Vect Real) forcePerMeterU

slcWght = quant (mkUid "W_i") (cn "weights")
  (S "the downward force per meter" `S.inThe` phrase zDir +:+
   S "on each slice caused by" +:+ phrase gravity)
  (vec cW) (Vect Real) forcePerMeterU

watrForce = quant (mkUid "H_i") (cn "interslice normal water forces")
  (S "the normal water forces per meter" `S.inThe` phrase zDir +:+
   S "exerted" `S.inThe` phrase xDir +:+ S "between each pair" `S.of_` S "adjacent slices")
  (vec cH) (Vect Real) forcePerMeterU

intShrForce = quant (mkUid "X_i") (cn "interslice shear forces")
  (S "the shear forces per meter" `S.inThe` phrase zDir +:+ S "exerted between adjacent slices")
  (vec cX) (Vect Real)forcePerMeterU

baseHydroForce = quant (mkUid "U_b,i") (cn "base hydrostatic forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "from water pressure within each slice")
  (sub (vec cU) lBase) (Vect Real) forcePerMeterU

surfHydroForce = quant (mkUid "U_t,i") (cn "surface hydrostatic forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "from water pressure acting" +:+
   S "into each slice from standing water" `S.onThe` S "slope surface")
  (sub (vec cU) lSurface) (Vect Real) forcePerMeterU

totNrmForce = quant (mkUid "N_i") (cn "normal forces")
  (S "the total reactive forces per meter" `S.inThe` phrase zDir +:+
   S "for each slice" `S.ofA` S "soil surface subject to a body resting on it")
  (vec cN) (Vect Real) forcePerMeterU

nrmFSubWat = quant (mkUid "N'_i") (cn "effective normal forces")
  (S "the forces per meter" `S.inThe` phrase zDir `S.for` S "each slice" `S.ofA` S "soil surface" `sC`
   S "subtracting pore water reactive force from total reactive force")
  (vec (prime $ variable "N")) (Vect Real) forcePerMeterU

surfLoad = quant (mkUid "Q_i") (cn "external forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+
   S "acting into the surface from the midpoint" `S.of_` S "each slice")
  (vec cQ) (Vect Real) forcePerMeterU

baseAngle = quant (mkUid "alpha_i") (cn "base angles")
  (S "the angles between the base" `S.of_` S "each slice and the horizontal")
  (vec lAlpha) (Vect Real) degree

surfAngle = quant (mkUid "beta_i") (cn "surface angles")
  (S "the angles between the surface" `S.of_` S "each slice and the horizontal")
  (vec lBeta) (Vect Real) degree

impLoadAngle = quant (mkUid "omega_i") (cn "imposed load angles")
  (S "the angles between the external force acting into the surface" `S.of_` S "each slice and the vertical")
  (vec lOmega) (Vect Real) degree

baseWthX = quant (mkUid "b_i") (cn "base width of slices")
  (S "the width" `S.of_` S "each slice" `S.inThe` phrase xDir)
  (vec lB) (Vect Real) metre

baseLngth = quant (mkUid "l_b,i") (cn "total base lengths of slices")
  (S "the lengths of each slice in the direction parallel to the slope of the base")
  (sub (vec cL) lB) (Vect Real) metre

surfLngth = quant (mkUid "l_s,i") (cn "surface lengths of slices")
  (S "the lengths" `S.of_` S "each slice" `S.inThe` S "direction parallel" `S.toThe` S "slope" `S.ofThe` S "surface")
  (sub (vec cL) lS) (Vect Real) metre

midpntHght = quant (mkUid "h_i")
  (compoundPhrase (yDir ^. term) (cn "heights of slices"))
  (S "the heights" `S.inThe` phrase yDir +:+ S "from the base" `S.of_` S "each slice" `S.toThe`
   S "slope surface" `sC` S "at the" +:+ phrase xDir +:+ S "midpoint" `S.ofThe` S "slice")
  (vec lH) (Vect Real) metre

porePressure = quant (mkUid "u") (cn "pore pressure")
  (S "the pressure that comes from water within the soil") lU Real pascal

shrStress = quant (mkUid "tau_i") (cn "shear strength")
  (S "the strength" `S.ofA` S "material against shear failure") (sup lTau (label "f")) Real pascal

sliceHght = quant (mkUid "h_z,i") (cn "heights of interslice normal forces")
  (D.toSent (pluralNP (height `inThePS` yDir)) `S.the_ofThe` S "interslice normal forces on each slice")
  (subZ (vec lH)) Real metre

sliceHghtW = quant (mkUid "h_z,w,i") (cn "heights of the water table")
  (S "the heights" `S.inThe` phrase yDir +:+ S "from the base" `S.of_` S "each slice" `S.toThe` S "water table")
  (sub (vec lH) lHeights) Real metre

nrmShearNum = quant (mkUid "C_num,i") (cn "proportionality constant numerator")
  (S $ "values for each slice that sum together to form the numerator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lNum) (Vect Real) newton

nrmShearDen = quant (mkUid "C_den,i") (cn "proportionality constant denominator")
  (S $ "values for each slice that sum together to form the denominator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lDen) (Vect Real) newton

fx = quant (mkUid "fx") (xCoord `ofThe` force)
  (S "the force acting" `S.inThe` phrase xDir) (subX cF) Real newton

fy = quant (mkUid "fy") (yCoord `ofThe` force)
  (S "the force acting" `S.inThe` phrase yDir) (subY cF) Real newton

fn = quant (mkUid "F_n") (cn "total normal force") (S "component" `S.ofA` S "force" `S.inThe` S "normal direction")
  (sub cF (label "n")) Real newton

ft = quant (mkUid "F_t") (cn "tangential force") (S "component" `S.ofA` S "force" `S.inThe` S "tangential direction")
  (sub cF (label "t")) Real newton

nrmForceSum = quant (mkUid "F_x^G") (cn "sums of the interslice normal forces")
  (S "the sums" `S.ofThe` S "normal forces acting on each pair" `S.of_` S "adjacent interslice boundaries")
  (sup (subX (vec cF)) lNorm) Real newton

watForceSum = quant (mkUid "F_x^H") (cn "sums of the interslice normal water forces")
  (S "the sums" `S.ofThe` S "normal water forces acting on each pair" `S.of_` S "adjacent interslice boundaries")
  (sup (subX (vec cF)) lNormWat) Real newton

sliceHghtRight = quant (mkUid "h^R") (cn "heights of the right side of slices")
  (S "the heights" `S.ofThe` S "right side" `S.of_` S "each slice" `sC` S "assuming slice surfaces have negative slope")
  (sup (vec lH) lRight) (Vect Real) metre

sliceHghtLeft = quant (mkUid "h^L") (cn "heights of the left side of slices")
  (S "the heights" `S.ofThe` S "left side" `S.of_` S "each slice" `sC` S "assuming slice surfaces have negative slope")
  (sup (vec lH) lLeft) (Vect Real) metre

totNormStress = quant (mkUid "sigma") (cn' "total normal stress")
  (S "the total force per area acting" `S.onThe` S "soil mass") lSigma Real pascal

tangStress = quant (mkUid "tau") (cn' "tangential stress")
  (S "the shear force per unit area") lTau Real pascal

effectiveStress = quant (mkUid "sigma'") (cn' "effective stress")
  (S $ "the stress in a soil mass that is effective in causing volume changes " ++
   "and mobilizes the shear strength arising from friction; represents the " ++
   "average stress carried by the soil skeleton")
  (prime lSigma) Real pascal

effNormStress = quant (mkUid "sigmaN'") (nounPhraseSP "effective normal stress")
  (S $ "the normal stress in a soil mass that is effective in causing volume " ++
   "changes; represents the average normal stress carried by the soil skeleton")
  (prime $ sub lSigma cN) Real pascal

dryVol = quant (mkUid "V_dry") (cn "volumes of dry soil")
  (S "the amount" `S.of_` S "space occupied by dry soil" `S.for` S "each slice")
  (sub (vec cV) lDry) Real m_3

satVol = quant (mkUid "V_sat") (cn "volumes of saturated soil")
  (S "the amount" `S.of_` S "space occupied by saturated soil" `S.for` S "each slice")
  (sub (vec cV) lSat) Real m_3

rotForce = quant (mkUid "F_rot") (cn "force causing rotation")
  (S "a force" `S.inThe` S "direction" `S.of_` S "rotation") (sub cF lRot) Real newton

momntArm = quant (mkUid "r") (cn' "length of the moment arm")
  (S "the distance between a force causing rotation and the axis of rotation")
  lR Real metre

----------------------
-- Unitless Symbols --
----------------------

unitless :: [DefinedQuantityDict]
unitless = [earthqkLoadFctr, normToShear, scalFunc, numbSlices, minFunction,
  mobShrC, shrResC, index, pi_, varblV, fsMin, unitVectj]

earthqkLoadFctr, normToShear, scalFunc, numbSlices,
  minFunction, mobShrC, shrResC, index, varblV :: DefinedQuantityDict

earthqkLoadFctr = quantNoUnit (mkUid "K_c") (nounPhraseSP "seismic coefficient")
  (S ("the proportionality factor of force that weight pushes outwards; " ++
   "caused by seismic earth movements"))
  (sub cK lCoeff) Real

normToShear = quantNoUnit (mkUid "lambda") (nounPhraseSP "proportionality constant")
  (S "the ratio of the interslice normal to the interslice shear force")
  lLambda Real

scalFunc = quantNoUnit (mkUid "f_i")
  (nounPhraseSP "interslice normal to shear force ratio variation function")
  (S "a function" `S.of_` D.toSent (phraseNP (distance `inThe` xDir)) +:+
   S "that describes the variation" `S.ofThe` S "interslice normal to shear ratio")
  (vec lF) Real

-- As we're going to subtract from this, can't type it 'Natural'.
numbSlices = quantNoUnit (mkUid "n") (nounPhraseSP "number of slices")
  (S "the number of slices into which the slip surface is divided")
  lN Integer

-- horrible hack, but it's only used once, so...
minFunction = quantNoUnit (mkUid "Upsilon") (nounPhraseSP "minimization function")
  (S "generic minimization function or algorithm")
  cUpsilon (mkFunction (replicate 10 Real) Real)

mobShrC = quantNoUnit (mkUid "Psi")
  (nounPhraseSP "second function for incorporating interslice forces into shear force")
  (S ("the function for converting mobile shear " ++ wiif ++
   ", to a calculation considering the interslice forces"))
  (vec cPsi) (Vect Real)

shrResC = quantNoUnit (mkUid "Phi")
  (nounPhraseSP "first function for incorporating interslice forces into shear force")
  (S ("the function for converting resistive shear " ++ wiif ++
   ", to a calculation considering the interslice forces"))
  (vec cPhi) (Vect Real)

--------------------
-- Index Function --
--------------------

varblV = quantNoUnit (mkUid "varblV") (nounPhraseSP "local index")
  (S "used as a bound variable index in calculations")
  lV Natural

-- As we do arithmetic on index, must type it 'Integer' right now
index = quantNoUnit (mkUid "index") (nounPhraseSP "index")
  (S "a number representing a single slice")
  lI Integer

-- FIXME: move to drasil-lang
indx1 :: (ExprC r, LiteralC r, Quantity a) => a -> r
indx1 a = idx (sy a) (int 1)

indxn :: (ExprC r, Quantity a) => a -> r
indxn a = idx (sy a) (sy numbSlices)

inxi, inxiP1, inxiM1 :: (ExprC r, LiteralC r, Quantity e) => e -> r
inxiP1 e = inx e 1
inxi   e = inx e 0
inxiM1 e = inx e (-1)

inx :: (ExprC r, LiteralC r, Quantity e) => e -> Integer -> r
inx e n
  | n < 0     = idx (sy e) (sy index $- int (-n))
  | n == 0    = idx (sy e) (sy index)
  | otherwise = idx (sy e) (sy index $+ int n)

sum1toN :: (ExprC r, LiteralC r) => r -> r
sum1toN = defsum (eqSymb index) (int 1) (sy numbSlices)

-- Labels

lBase, lCoeff, lCoords, lCSlip, lDen, lDry, lHeights, lLeft, lMaxEtr, lMaxExt,
  lMinEtr, lMinExt, lNorm, lNormWat, lNum, lRight, lRot, lSafety, lSat, lSlip,
  lSlope, lSurface, lWatTab :: Symbol
lBase    = label "b"
lCoeff   = label "c"
lCoords  = label "(x,y)"
lCSlip   = label "cs"
lDen     = label "den"
lDry     = label "dry"
lHeights = label "z,w"
lLeft    = label "L"
lMaxEtr  = label "maxEtr"
lMaxExt  = label "maxExt"
lMinEtr  = label "minEtr"
lMinExt  = label "minExt"
lNorm    = label "G"
lNormWat = label "H"
lNum     = label "num"
lRight   = label "R"
lRot     = label "rot"
lSafety  = label "S"
lSat     = label "sat"
lSlip    = label "slip"
lSlope   = label "slope"
lSurface = label "g"
lWatTab  = label "wt"
