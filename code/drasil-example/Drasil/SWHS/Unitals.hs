module Drasil.SWHS.Unitals where -- all of this file is exported

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (mkQuantDef)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (simulation)
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Quantities.Math (gradient, pi_, surArea, surface, uNormalVect)
import Data.Drasil.Quantities.PhysicalProperties (mass, density, vol)
import Data.Drasil.Quantities.Physics (subMax, subMin, supMax, supMin, time)
import Data.Drasil.Quantities.Thermodynamics (sensHeat, temp, meltPt,
  htFlux, latentHeat, boilPt, heatCapSpec)
import Data.Drasil.SI_Units (m_2, second, kilogram, metre, joule,
  centigrade, m_3, specificE)
import Data.Drasil.Units.PhysicalProperties (densityU)
import qualified Data.Drasil.Units.Thermodynamics as UT (heatTransferCoef,
  heatCapSpec, thermalFlux, volHtGenU)

import Drasil.SWHS.Concepts (water)

import Control.Lens ((^.))

symbols :: [DefinedQuantityDict]
symbols = pi_ : map dqdWr units ++ map dqdWr unitless ++ map dqdWr constrained

symbolsAll :: [QuantityDict]
symbolsAll = map qw symbols ++ map qw specParamValList ++
  map qw [htFusionMin, htFusionMax, coilSAMax] ++
  map qw [absTol, relTol]

-- Symbols with Units --

units :: [UnitaryConceptDict]
units = map ucw [inSA, outSA, heatCapSpec, htCapL,
  htCapS, htCapV, sensHeat, pcmInitMltE,
  volHtGen, htTransCoeff, pcmMass, wMass, htFlux, latentHeat,
  thFluxVect, htFluxC, htFluxIn, htFluxOut, htFluxP, latentEP,
  temp, boilPt, tempEnv, meltPt, tInitMelt,
  tFinalMelt, vol, tankVol, wVol, deltaT,
  density, tau, tauLP, tauSP, tauW, thickness] ++
  map ucw [mass, time] -- ++ [tankLength, diam, coilSA]

unitalChuncks :: [UnitalChunk]
unitalChuncks = [inSA, outSA, htCapL, htCapS, htCapV,
  pcmInitMltE, volHtGen, htTransCoeff,
  pcmMass, wMass,
  thFluxVect, htFluxC, htFluxIn, htFluxOut, htFluxP, latentEP,
  tempEnv, tInitMelt,
  tFinalMelt, tankVol, wVol, deltaT,
  tau, tauLP, tauSP, tauW, simTime, thickness]

inSA, outSA, htCapL, htCapS, htCapV,
  pcmInitMltE, volHtGen, htTransCoeff,
  pcmMass, wMass,
  thFluxVect, htFluxC, htFluxIn, htFluxOut, htFluxP, latentEP,
  tempEnv, tInitMelt,
  tFinalMelt, tankVol, wVol, deltaT,
  tau, tauLP, tauSP, tauW, simTime, thickness:: UnitalChunk

---------------------
-- Regular Symbols --
---------------------

--symbol names can't begin with a capital

inSA = uc' "inSA" (nounPhraseSP
  "surface area over which heat is transferred in")
  "Surface area over which thermal energy is transferred into an object"
  (sub cA lIn) m_2

outSA = uc' "outSA" (nounPhraseSP
  "surface area over which heat is transferred out")
  "Surface area over which thermal energy is transferred out of an object"
  (sub cA lOut) m_2

htCapL = uc' "htCapL" (nounPhraseSP "specific heat capacity of a liquid")
  ("The amount of energy required to raise the temperature of a given " ++
  "unit mass of a given liquid by a given amount")
  (sup (eqSymb heatCapSpec) lLiquid) UT.heatCapSpec

htCapS = uc' "htCapS"
  (nounPhraseSP "specific heat capacity of a solid")
  ("The amount of energy required to raise the temperature of " ++
  "a given unit mass of a given solid by a given amount")
  (sup (eqSymb heatCapSpec) lSolid) UT.heatCapSpec

htCapV = uc' "htCapV"
  (nounPhraseSP "specific heat capacity of a vapour")
  ("The amount of energy required to raise the temperature of a given " ++
  "unit mass of vapour by a given amount")
  (sup (eqSymb heatCapSpec) lVapour) UT.heatCapSpec

pcmInitMltE = uc' "pcmInitMltE" (nounPhraseSP
  "change in heat energy in the PCM at the instant when melting begins")
  "Change in thermal energy in the phase change material at the melting point"
  (sup (sub (sub (eqSymb sensHeat) lPCM) lMelt) lInit) joule

volHtGen = uc' "volHtGen"
  (nounPhraseSP "volumetric heat generation per unit volume")
  "Amount of thermal energy generated per unit volume" lG UT.volHtGenU

htTransCoeff = uc' "htTransCoeff"
  (nounPhraseSP "convective heat transfer coefficient")
  ("The proportionality constant between the heat flux and the " ++
  "thermodynamic driving force for the flow of thermal energy")
  lH UT.heatTransferCoef

pcmMass = uc' "pcmMass" (nounPhraseSP "mass of phase change material")
  "The quantity of matter within the phase change material"
  (sub (eqSymb mass) lPCM) kilogram

wMass = uc' "wMass" (nounPhraseSP "mass of water")
  "The quantity of matter within the water" (sub (eqSymb mass) lWater) kilogram

thFluxVect = uc' "thFluxVect" (nounPhraseSP "thermal flux vector")
  "Vector denoting the direction of thermal flux through a surface"
  (vec lQ) UT.thermalFlux

htFluxC = uc' "htFluxC"
  (nounPhraseSP "heat flux into the water from the coil")
  "The rate of heat energy transfer into the water from the coil per unit time"
  (sub (eqSymb htFlux) lCoil) UT.thermalFlux

htFluxIn = uc' "htFluxIn" (nounPhraseSP "heat flux input")
  "The rate of heat energy transfer into an object per unit time"
  (sub (eqSymb htFlux) lIn) UT.thermalFlux

htFluxOut = uc' "htFluxOut" (nounPhraseSP "heat flux output")
  "The rate of heat energy transfer into an object per unit time"
  (sub (eqSymb htFlux) lOut) UT.thermalFlux

htFluxP = uc' "htFluxP" (nounPhraseSP "heat flux into the PCM from water")
  ("The rate of heat energy transfer into the phase" ++
  "change material from the water per unit time")
  (sub (eqSymb htFlux) lPCM) UT.thermalFlux

latentEP = uc' "latentEP" (nounPhraseSP "latent heat energy added to PCM")
  ("Energy released or absorbed, by a body or a thermodynamic system, "++
  "during a constant-temperature process and absorbed by the phase" ++
  "change material") (sub (eqSymb latentHeat) lPCM) joule

tempEnv = uc' "tempEnv" (nounPhraseSP "temperature of the environment")
  "The tempature of a given environment"
  (sub (eqSymb temp) lEnv) centigrade

tInitMelt = uc' "tInitMelt"
  (nounPhraseSP "time at which melting of PCM begins")
  ("Time at which the phase change material " ++
    "begins changing from a solid to a liquid")
  (sup (sub (eqSymb time) lMelt) lInit) second

tFinalMelt = uc' "tFinalMelt"
  (nounPhraseSP "time at which melting of PCM ends")
  ("Time at which the phase change material " ++
    "finishes changes from a solid to a liquid")
  (sup (sub (eqSymb time) lMelt) lFinal) second
  
tankVol = uc' "tankVol" (nounPhraseSP "volume of the cylindrical tank")
  "The amount of space encompassed by a tank"
  (sub (eqSymb vol) lTank) m_3

wVol = uc' "wVol" (vol `of_` water)
  "The amount of space occupied by a given quantity of water"
  (sub (eqSymb vol) lWater) m_3

deltaT = uc' "deltaT" (nounPhraseSP "change in temperature")
  "Change in the average kinetic energy of a given material"
  (Concat [cDelta, eqSymb temp]) centigrade

tau = uc' "tau" (nounPhraseSP "dummy variable for integration over time")
  "Binary value representing the presence or absence of integration over time"
  lTau second
--Not sure how to define anything after this point

tauLP = uc' "tauLP" (nounPhraseSP "ODE parameter for liquid PCM")
  ("Derived through melting of phase change material, which " ++
  "changes ODE parameter for solid PCM into parameter for liquid")
  (sup (sub lTau lPCM) lLiquid) second

tauSP = uc' "tauSP" (nounPhraseSP "ODE parameter for solid PCM")
  "Derived parameter based on rate of change of temperature of phase change material"
  (sup (sub lTau lPCM) lSolid) second

tauW = uc' "tauW" (nounPhraseSP "ODE parameter for water related to decay time")
  "Derived parameter based on rate of change of temperature of water"
  (sub lTau lWater) second

simTime = uc' "simTime" (compoundPhrase' (simulation ^. term)
  (time ^. term)) "Time over which the simulation runs"
  lT second

thickness = uc'  "thickness" (nounPhraseSP "Minimum thickness of a sheet of PCM")
  "The minimum thickness of a sheet of PCM"
  (subMin lH) metre
----------------------
-- Unitless symbols --
----------------------

-- FIXME: this list should not be hand-constructed
unitless :: [DefinedQuantityDict]
unitless = [uNormalVect, dqdWr surface, eta, meltFrac, gradient, fracMin, consTol,
            aspectRatio, aspectRatioMin, aspectRatioMax]

eta, meltFrac, fracMin, consTol, aspectRatio, aspectRatioMin, aspectRatioMax :: DefinedQuantityDict

-- FIXME: should this have units?
eta = dqd' (dcc "eta" (nounPhraseSP "ODE parameter related to decay rate")
  "Derived parameter based on rate of change of temperature of water")
  (const lEta) Real Nothing

meltFrac = dqd' (dcc "meltFrac" (nounPhraseSP "melt fraction")
  "Ratio of thermal energy to amount of mass melted")
  --FIXME: Not sure if definition is exactly correct
  (const lPhi) Real Nothing

fracMin = dqd' (dcc "fracMin" 
  (nounPhraseSP "minimum fraction of the tank volume taken up by the PCM")
  "minimum fraction of the tank volume taken up by the PCM")
   (const $ Variable "MINFRACT") Real Nothing

consTol = dqd' (dcc "consTol" 
  (nounPhraseSP "relative tolerance for conservation of energy") 
  "relative tolerance for conservation of energy")
  (const $ sub cC lTol) Real Nothing

aspectRatio = dqd' (dcc "aspectRatio" 
  (nounPhraseSP "aspect ratio")
  "ratio of tank diameter to tank length")
   (const $ Variable "AR") Real Nothing

aspectRatioMin = dqd' (dcc "aspectRatioMin" 
  (nounPhraseSP "minimum aspect ratio") "minimum aspect ratio")
   (const $ subMin (eqSymb aspectRatio)) Real Nothing

aspectRatioMax = dqd' (dcc "aspectRatioMax" 
  (nounPhraseSP "maximum aspect ratio") "maximum aspect ratio")
   (const $ subMax (eqSymb aspectRatio)) Real Nothing

-----------------
-- Constraints --
-----------------

constrained :: [ConstrConcept]
constrained = map cnstrw' inputConstraints ++ map cnstrw' outputs

-- Input Constraints
inputs :: [QuantityDict]
inputs = map qw inputConstraints ++ map qw [absTol, relTol]

inputConstraints :: [UncertQ]
inputConstraints = [tankLength, diam, pcmVol, pcmSA, pcmDensity,
  tempMeltP, htCapSP, htCapLP, htFusion, coilSA, tempC,
  wDensity, htCapW, coilHTC, pcmHTC, tempInit, timeStep, timeFinal]

tankLength, diam, pcmVol, pcmSA, pcmDensity, tempMeltP,
  htCapSP, htCapLP, htFusion, coilSA, tempC, wDensity,
  htCapW, coilHTC, pcmHTC, tempInit, timeStep, timeFinal :: UncertQ

tempPCM, tempW, watE, pcmE :: ConstrConcept

-- Constraint 1
tankLength = uqc "tankLength" (nounPhraseSP "length of tank")
  "The length of the tank" cL metre Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy tankLengthMin) (Inc, sy tankLengthMax)] (dbl 1.5)
  defaultUncrt

-- Constraint 2
diam = uqc "diam" (nounPhraseSP "diameter of tank")
  "The diameter of the tank" cD metre Rational
  [gtZeroConstr, sfwrc $ Bounded (Inc, sy arMin) (Inc, sy arMax)]
  (dbl 0.412) defaultUncrt

-- Constraint 3
pcmVol = uqc "pcmVol" (nounPhraseSP "volume of PCM")
  "The amount of space occupied by a given quantity of phase change material"
  (sub (eqSymb vol) lPCM) m_3 Rational
  [physc $ Bounded (Exc,0) (Exc, sy tankVol),
   sfwrc $ UpFrom (Inc, sy fracMin * sy tankVol)] 
  (dbl 0.05) defaultUncrt
  -- needs to add (D,L)*minfract to end of last constraint

-- Constraint 4
-- Capitalization Issue here too.
pcmSA = uqc "pcmSA"
  (compoundPhrase (nounPhrase'' (S "phase change material")
  (S "phase change material")
  CapFirst CapWords) (nounPhrase'' (phrase surArea) (phrase surArea)
  CapFirst CapWords))
  "Area covered by the outermost layer of the phase change material"
  (sub cA lPCM) m_2 Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy pcmVol) (Inc, (2 / sy thickness) * sy tankVol)]
  (dbl 1.2) defaultUncrt

-- Constraint 5
pcmDensity = uq (cuc'' "pcmDensity" (nounPhraseSP "density of PCM")
  "Mass per unit volume of the phase change material"
  (autoStage $ sub (eqSymb density) lPCM) densityU Rational
  [gtZeroConstr, sfwrc $ Bounded (Exc, sy pcmDensityMin) (Exc, sy pcmDensityMax)]
  (dbl 1007)) defaultUncrt

-- Constraint 6
tempMeltP = uqc "tempMeltP"
  (nounPhraseSP "melting point temperature for PCM")
  ("Temperature at which the phase change " ++
    "material transitions from a solid to a liquid")
  (sup (sub (eqSymb temp) lMelt) lPCM) centigrade Rational
  [physc $ Bounded (Exc,0) (Exc, sy tempC)] (dbl 44.2) defaultUncrt

-- Constraint 7
htCapSP = uqc "htCapSP"
  (nounPhraseSP "specific heat capacity of PCM as a solid")
  ("The amount of energy required to raise the temperature of a " ++
  "given unit mass of solid phase change material by a given amount")
  (sup (sub (eqSymb heatCapSpec) lPCM) lSolid) UT.heatCapSpec Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htCapSPMin) (Exc, sy htCapSPMax)]
  (dbl 1760) defaultUncrt

-- Constraint 8
htCapLP = uqc "htCapLP"
  (nounPhraseSP "specific heat capacity of PCM as a liquid")
  ("The amount of energy required to raise the temperature of a " ++
  "given unit mass of liquid phase change material by a given amount")
  (sup (sub (eqSymb heatCapSpec) lPCM) lLiquid) UT.heatCapSpec Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htCapLPMin) (Exc, sy htCapLPMax )]
  (dbl 2270) defaultUncrt

--Constraint 9
htFusion = uqc "htFusion" (nounPhraseSP "specific latent heat of fusion")
  ("amount of thermal energy required to " ++
  "completely melt a unit mass of a substance")
  (sub cH lFusion) specificE Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htFusionMin) (Exc, sy htFusionMax)] (dbl 211600) defaultUncrt

-- Constraint 10
-- The "S "heating coil" " should be replaced by "phrase coil",
-- Since the capitalization issue, they are replaced by S so far.
coilSA = uqc "coilSA"
  (compoundPhrase (nounPhrase'' (S "heating coil") (S "heating coil") CapFirst CapWords)
  (nounPhrase'' (phrase surArea) (phrase surArea) CapFirst CapWords))
  "Area covered by the outermost layer of the coil" (sub cA lCoil) m_2 Rational
  [gtZeroConstr,
  sfwrc $ UpTo (Inc, sy coilSAMax)] (dbl 0.12) defaultUncrt

-- Constraint 11
tempC = uqc "tempC" (nounPhraseSP "temperature of the heating coil")
  "The average kinetic energy of the particles within the coil"
  (sub (eqSymb temp) lCoil) centigrade Rational
  [physc $ Bounded (Exc,0) (Exc,100)] (dbl 50) defaultUncrt

-- Constraint 12
wDensity = uq (cuc'' "wDensity" (density `of_` water)
  "Mass per unit volume of water" (autoStage $ sub (eqSymb density) lWater) densityU Rational
  [gtZeroConstr, sfwrc $ Bounded (Exc, sy wDensityMin) (Inc, sy wDensityMax)]
  (dbl 1000)) defaultUncrt

-- Constraint 13
htCapW = uqc "htCapW" (heatCapSpec `of_` water)
  ("The amount of energy required to raise the " ++
    "temperature of a given unit mass of water by a given amount")
  (sub (eqSymb heatCapSpec) lWater) UT.heatCapSpec Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htCapWMin) (Exc, sy htCapWMax)] (dbl 4186) defaultUncrt
  
-- Constraint 14
coilHTC = uqc "coilHTC" (nounPhraseSP
  "convective heat transfer coefficient between coil and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the coil to the surrounding water")
  (sub (eqSymb htTransCoeff) lCoil)
  UT.heatTransferCoef Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy coilHTCMin) (Inc, sy coilHTCMax)] (dbl 1000) defaultUncrt

-- Constraint 15
pcmHTC = uqc "pcmHTC"
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the phase change material to the surrounding water")
  (sub lH lPCM) UT.heatTransferCoef Rational
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy pcmHTCMin) (Inc, sy pcmHTCMax)] (dbl 1000) defaultUncrt
  
-- Constraint 16
tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (eqSymb temp) lInit) centigrade Rational
  [physc $ Bounded (Exc,0) (Exc, sy meltPt)] (dbl 40) defaultUncrt
  
-- Constraint 17
timeFinal = uqc "timeFinal" (nounPhraseSP "final time")
  ("The amount of time elapsed from the beginning of the " ++
  "simulation to its conclusion") (sub (eqSymb time) 
  lFinal) second Rational
  [gtZeroConstr,
  sfwrc $ UpTo (Exc, sy timeFinalMax)] (dbl 50000) defaultUncrt

timeStep = uqc "timeStep" (nounPhraseSP "time step for simulation")
  ("The finite discretization of time used in the numerical method" ++
    "for solving the computational model")
  (sub (eqSymb time) lStep) second Rational
  [physc $ Bounded (Exc,0) (Exc, sy timeFinal)]
  (dbl 0.01) defaultUncrt
  
-- Output Constraints
outputs :: [ConstrConcept]
--FIXME: Add typical values or use Nothing if not known
outputs = [tempW, tempPCM, watE, pcmE]

-- Constraint 18
tempW = cuc' "tempW"
  (nounPhraseSP "temperature of the water")
  "The average kinetic energy of the particles within the water" 
  (sub (eqSymb temp) lWater) centigrade Rational
  [physc $ Bounded (Inc, sy tempInit) (Inc, sy tempC)] (dbl 0)

-- Constraint 19
tempPCM = cuc' "tempPCM"
  (nounPhraseSP "temperature of the phase change material" )
  ("The average kinetic energy of the " ++
    "particles within the phase change material")
  (sub (eqSymb temp) lPCM) centigrade Rational
  [physc $ Bounded (Inc, sy tempInit) (Inc, sy tempC)] (dbl 0)
  
-- Constraint 20
watE = cuc' "watE" (nounPhraseSP "change in heat energy in the water")
  "Change in thermal energy within the water" 
  (sub (eqSymb sensHeat) lWater) joule Rational
  [physc $ UpFrom (Inc,0)] (dbl 0)
  
-- Constraint 21
pcmE = cuc' "pcmE" (nounPhraseSP "change in heat energy in the PCM")
  "Change in thermal energy within the phase change material" 
  (sub (eqSymb sensHeat) lPCM) joule Rational
  [physc $ UpFrom (Inc, 0)] (dbl 0)

---------------------------------
-- Uncertainties with no Units --
---------------------------------

absTol, relTol :: UncertainChunk

absTol = uvc "absTol" (nounPhraseSP "absolute tolerance") 
  (sub cA lTol) Real
  [ physc $ Bounded (Exc,0) (Exc,1)] 
   (dbl (10.0**(-10))) (uncty 0.01 Nothing)

relTol = uvc "relTol" (nounPhraseSP "relative tolerance") 
  (sub cR lTol) Real
  [ physc $ Bounded (Exc,0) (Exc,1)] 
  (dbl (10.0**(-10))) (uncty 0.01 Nothing)

-------------------------
-- Max / Min Variables --
-------------------------

specParamValList :: [QDefinition]
specParamValList = [tankLengthMin, tankLengthMax, pcmDensityMin, pcmDensityMax,
  wDensityMin, wDensityMax, htCapSPMin, htCapSPMax, htCapLPMin, htCapLPMax,
  htFusionMin, htFusionMax, coilSAMax, htCapWMin, htCapWMax, coilHTCMin,
  coilHTCMax, pcmHTCMin, pcmHTCMax, timeFinalMax, fracMinAux, consTolAux,
  arMin, arMax]

tankLengthMin, tankLengthMax, pcmDensityMin, 
  pcmDensityMax, wDensityMin, wDensityMax, htCapSPMin, htCapSPMax, htCapLPMin,
  htCapLPMax, htFusionMin, htFusionMax, coilSAMax, htCapWMin, htCapWMax,
  coilHTCMin, coilHTCMax, pcmHTCMin, pcmHTCMax, timeFinalMax, fracMinAux,
  consTolAux, arMin, arMax :: QDefinition

consTolAux = mkQuantDef consTol $ perc 1 5

-- Used in Constraint 1
tankLengthMin = mkQuantDef (unitary "tankLengthMin"
  (nounPhraseSP "minimum length of tank")
  (subMin (eqSymb tankLength)) metre Rational) (dbl 0.1)

tankLengthMax = mkQuantDef (unitary "tankLengthMax"
  (nounPhraseSP "maximum length of tank")
  (subMax (eqSymb tankLength)) metre Rational) 50

fracMinAux = mkQuantDef fracMin $ dbl 1.0e-6

arMin = mkQuantDef aspectRatioMin $ dbl 0.01
arMax = mkQuantDef aspectRatioMax 100

-- Used in Constraint 5
pcmDensityMin = mkQuantDef (unitary' "pcmDensityMin"
  (nounPhraseSP "minimum density of PCM")
  (autoStage $ supMin (eqSymb pcmDensity)) densityU Rational) 500

pcmDensityMax = mkQuantDef (unitary' "pcmDensityMax"
  (nounPhraseSP "maximum density of PCM")
  (autoStage $ supMax (eqSymb pcmDensity)) densityU Rational) 20000

-- Used in Constraint 7
htCapSPMin = mkQuantDef (unitary "htCapSPMin"
  (nounPhraseSP "minimum specific heat capacity of PCM as a solid")
  (subMin (eqSymb htCapSP)) UT.heatCapSpec Rational) 100

htCapSPMax = mkQuantDef (unitary "htCapSPMax"
  (nounPhraseSP "maximum specific heat capacity of PCM as a solid")
  (subMax (eqSymb htCapSP)) UT.heatCapSpec Rational) 4000

-- Used in Constraint 8
htCapLPMin = mkQuantDef (unitary "htCapLPMin"
  (nounPhraseSP "minimum specific heat capacity of PCM as a liquid")
  (subMin (eqSymb htCapLP)) UT.heatCapSpec Rational) 100

htCapLPMax = mkQuantDef (unitary "htCapLPMax"
  (nounPhraseSP "maximum specific heat capacity of PCM as a liquid")
  (subMax (eqSymb htCapLP)) UT.heatCapSpec Rational) 5000

-- Used in Constraint 9
htFusionMin = mkQuantDef (unitary "htFusionMin"
  (nounPhraseSP "minimum specific latent heat of fusion")
  (subMin (eqSymb htFusion)) UT.heatCapSpec Rational) 0 

htFusionMax = mkQuantDef (unitary "htFusionMax"
  (nounPhraseSP "maximum specific latent heat of fusion")
  (subMax (eqSymb htFusion)) UT.heatCapSpec Rational) 1000000 

-- Used in Constraint 10
coilSAMax = mkQuantDef (unitary "coilSAMax"
  (nounPhraseSP "maximum surface area of coil")
  (supMax (eqSymb coilSA)) m_2 Rational) 100000

-- Used in Constraint 12
wDensityMin = mkQuantDef (unitary' "wDensityMin"
  (nounPhraseSP "minimum density of water")
  (autoStage $ supMin (eqSymb wDensity)) densityU Rational) 950

wDensityMax = mkQuantDef (unitary' "wDensityMax"
  (nounPhraseSP "maximum density of water")
  (autoStage $ supMax (eqSymb wDensity)) densityU Rational) 1000
  
-- Used in Constraint 13
htCapWMin = mkQuantDef (unitary "htCapWMin"
  (nounPhraseSP "minimum specific heat capacity of water")
  (supMin (eqSymb htCapW)) UT.heatCapSpec Rational) 4170

htCapWMax = mkQuantDef (unitary "htCapWMax"
  (nounPhraseSP "maximum specific heat capacity of water")
  (supMax (eqSymb htCapW)) UT.heatCapSpec Rational) 4210

-- Used in Constraint 14
coilHTCMin = mkQuantDef (unitary "coilHTCMin"
  (nounPhraseSP $ "minimum convective heat " ++
  "transfer coefficient between coil and water")
  (supMin (eqSymb coilHTC)) UT.heatTransferCoef Rational) 10

coilHTCMax = mkQuantDef (unitary "coilHTCMax"
  (nounPhraseSP $ "maximum convective heat " ++
  "transfer coefficient between coil and water")
  (supMax (eqSymb coilHTC)) UT.heatTransferCoef Rational) 10000
  
-- Used in Constraint 15
pcmHTCMin = mkQuantDef (unitary "pcmHTCMin"
  (nounPhraseSP $ "minimum convective heat " ++
  "transfer coefficient between PCM and water")
  (supMin (eqSymb pcmHTC)) UT.heatTransferCoef Rational) 10

pcmHTCMax = mkQuantDef (unitary "pcmHTCMax"
  (nounPhraseSP $ "maximum convective heat " ++
  "transfer coefficient between PCM and water")
  (supMax (eqSymb pcmHTC)) UT.heatTransferCoef Rational) 10000
  
-- Used in Constraint 17
timeFinalMax = mkQuantDef (unitary "timeFinalMax"
  (nounPhraseSP "maximum final time")
  (supMax (eqSymb timeFinal)) second Rational) 86400

-- Labels
lCoil, lEnv, lFinal, lFusion, lIn, lInit, lLiquid, lMelt, lOut, lPCM, lSolid,
  lStep, lTank, lTol, lVapour, lWater :: Symbol
lCoil   = Label "C"
lEnv    = Label "env"
lFinal  = Label "final"
lFusion = Label "f"
lIn     = Label "in"
lInit   = Label "init"
lLiquid = Label "L"
lMelt   = Label "melt"
lOut    = Label "out"
lPCM    = Label "P"
lSolid  = Label "S"
lStep   = Label "step"
lTank   = Label "tank"
lTol    = Label "tol"
lVapour = Label "V"
lWater  = Label "W"