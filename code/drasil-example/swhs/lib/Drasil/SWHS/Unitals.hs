module Drasil.SWHS.Unitals (module Drasil.SWHS.Unitals) where -- all of this file is exported

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Display (Symbol(Atop), Decoration(Delta))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (simulation)
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Quantities.Math (gradient, pi_, surArea, surface, uNormalVect, area)
import Data.Drasil.Quantities.PhysicalProperties (mass, density, vol)
import Data.Drasil.Quantities.Physics (subMax, subMin, supMax, supMin, time)
import Data.Drasil.Quantities.Thermodynamics (sensHeat, temp, meltPt,
  htFlux, latentHeat, boilPt, heatCapSpec)
import Data.Drasil.SI_Units (m_2, second, kilogram, metre, joule,
  centigrade, m_3, specificE)
import Data.Drasil.Units.PhysicalProperties (densityU)
import qualified Data.Drasil.Units.Thermodynamics as UT (heatTransferCoef,
  heatCapSpec, thermalFlux, volHtGenU)

import Drasil.Database (mkUid)
import Drasil.SWHS.Concepts (water, phsChgMtrl)

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

symbols :: [DefinedQuantityDict]
symbols = pi_ : units ++ unitless ++ map dqdWr constrained
 ++ unitalChuncks ++ map dqdWr specParamValList ++
   map dqdWr [htFusionMin, htFusionMax, coilSAMax] ++
   map dqdWr [absTol, relTol]

-- Symbols with Units --

units :: [DefinedQuantityDict]
units = [surArea, area, sensHeat, htFlux, latentHeat, temp, boilPt, meltPt,
  vol, density, mass, time]

unitalChuncks :: [DefinedQuantityDict]
unitalChuncks = units ++ [inSA, outSA, htCapL, htCapS, htCapV,
  pcmInitMltE, volHtGen, htTransCoeff,
  pcmMass, wMass, heatCapSpec,
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
  tau, tauLP, tauSP, tauW, simTime, thickness:: DefinedQuantityDict

---------------------
-- Regular Symbols --
---------------------

--symbol names can't begin with a capital

inSA = quant (mkUid "inSA") (nounPhraseSP
  "surface area over which heat is transferred in")
  (S "surface area over which thermal energy is transferred into an object")
  (sub cA lIn) Real m_2

outSA = quant (mkUid "outSA") (nounPhraseSP
  "surface area over which heat is transferred out")
  (S "surface area over which thermal energy is transferred out of an object")
  (sub cA lOut) Real m_2

htCapL = quant (mkUid "htCapL") (nounPhraseSP "specific heat capacity of a liquid")
  (S $ "the amount of energy required to raise the temperature of a given " ++
  "unit mass of a given liquid by a given amount")
  (sup (eqSymb heatCapSpec) lLiquid) Real UT.heatCapSpec

htCapS = quant (mkUid "htCapS")
  (nounPhraseSP "specific heat capacity of a solid")
  (S $ "the amount of energy required to raise the temperature of " ++
  "a given unit mass of a given solid by a given amount")
  (sup (eqSymb heatCapSpec) lSolid) Real UT.heatCapSpec

htCapV = quant (mkUid "htCapV")
  (nounPhraseSP "specific heat capacity of a vapour")
  (S $ "the amount of energy required to raise the temperature of a given " ++
  "unit mass of vapour by a given amount")
  (sup (eqSymb heatCapSpec) lVapour) Real UT.heatCapSpec

pcmInitMltE = quant (mkUid "pcmInitMltE") (nounPhraseSP
  "change in heat energy in the PCM at the instant when melting begins")
  (S "change in thermal energy in the phase change material at the melting point")
  (sup (sub (sub (eqSymb sensHeat) lPCM) lMelt) lInit) Real joule

volHtGen = quant (mkUid "volHtGen")
  (nounPhraseSP "volumetric heat generation per unit volume")
  (S "amount of thermal energy generated per unit volume") lG Real UT.volHtGenU

htTransCoeff = quant (mkUid "htTransCoeff")
  (nounPhraseSP "convective heat transfer coefficient")
  (S $ "the proportionality constant between the heat flux and the " ++
  "thermodynamic driving force for the flow of thermal energy")
  lH Real UT.heatTransferCoef

pcmMass = quant (mkUid "pcmMass") (nounPhraseSP "mass of phase change material")
  (S "the quantity of matter within the phase change material")
  (sub (eqSymb mass) lPCM) Real kilogram

wMass = quant (mkUid "wMass") (nounPhraseSP "mass of water")
  (S "the quantity of matter within the water") (sub (eqSymb mass) lWater) Real kilogram

thFluxVect = quant (mkUid "thFluxVect") (nounPhraseSP "thermal flux vector")
  (S "vector denoting the direction of thermal flux through a surface")
  (vec lQ) Real UT.thermalFlux

htFluxC = quant (mkUid "htFluxC")
  (nounPhraseSP "heat flux into the water from the coil")
  (S "the rate of heat energy transfer into the water from the coil per unit time")
  (sub (eqSymb htFlux) lCoil) Real UT.thermalFlux

htFluxIn = quant (mkUid "htFluxIn") (nounPhraseSP "heat flux input")
  (S "the rate of heat energy transfer into an object per unit time")
  (sub (eqSymb htFlux) lIn) Real UT.thermalFlux

htFluxOut = quant (mkUid "htFluxOut") (nounPhraseSP "heat flux output")
  (S "the rate of heat energy transfer into an object per unit time")
  (sub (eqSymb htFlux) lOut) Real UT.thermalFlux

htFluxP = quant (mkUid "htFluxP") (nounPhraseSP "heat flux into the PCM from water")
  (S $ "the rate of heat energy transfer into the phase" ++
  "change material from the water per unit time")
  (sub (eqSymb htFlux) lPCM) Real UT.thermalFlux

latentEP = quant (mkUid "latentEP") (nounPhraseSP "latent heat energy added to PCM")
  (S $ "energy released or absorbed, by a body or a thermodynamic system, "++
  "during a constant-temperature process and absorbed by the phase" ++
  "change material") (sub (eqSymb latentHeat) lPCM) Real joule

tempEnv = quant (mkUid "tempEnv") (nounPhraseSP "temperature of the environment")
  (S "the tempature of a given environment")
  (sub (eqSymb temp) lEnv) Real centigrade

tInitMelt = quant (mkUid "tInitMelt")
  (nounPhraseSP "time at which melting of PCM begins")
  (S $ "time at which the phase change material " ++
    "begins changing from a solid to a liquid")
  (sup (sub (eqSymb time) lMelt) lInit) Real second

tFinalMelt = quant (mkUid "tFinalMelt")
  (nounPhraseSP "time at which melting of PCM ends")
  (S $ "time at which the phase change material " ++
    "finishes changes from a solid to a liquid")
  (sup (sub (eqSymb time) lMelt) lFinal) Real second

tankVol = quant (mkUid "tankVol") (nounPhraseSP "volume of the cylindrical tank")
  (S "the amount of space encompassed by a tank")
  (sub (eqSymb vol) lTank) Real m_3

wVol = quant (mkUid "wVol") (vol `of_` water)
  (S "the amount of space occupied by a given quantity of water")
  (sub (eqSymb vol) lWater) Real m_3

deltaT = quant (mkUid "deltaT") (nounPhraseSP "change in temperature")
  (S "change in the average kinetic energy of a given material")
  (Atop Delta $ eqSymb temp) Real centigrade

tau = quant' (mkUid "tau") (nounPhraseSP "dummy variable for integration over time")
  (S "binary value representing the presence or absence of integration over time")
  (autoStage lTau) Real second
--Not sure how to define anything after this point

tauLP = quant' (mkUid "tauLP") (nounPhraseSP "ODE parameter for liquid PCM")
  (S $ "derived through melting of phase change material, which " ++
  "changes ODE parameter for solid PCM into parameter for liquid")
  (autoStage $ sup (sub lTau lPCM) lLiquid) Real second

tauSP = quant' (mkUid "tauSP") (nounPhraseSP "ODE parameter for solid PCM")
  (S "derived parameter based on rate of change of temperature of phase change material")
  (autoStage $ sup (sub lTau lPCM) lSolid) Real second

tauW = quant' (mkUid "tauW") (nounPhraseSP "ODE parameter for water related to decay time")
  (S "derived parameter based on rate of change of temperature of water")
  (autoStage $ sub lTau lWater) Real second

simTime = quant (mkUid "simTime") (compoundPhrase' (simulation ^. term)
  (time ^. term)) (S "time over which the simulation runs")
  lT Real second

thickness = quant  (mkUid "thickness") (nounPhraseSP "Minimum thickness of a sheet of PCM")
  (S "the minimum thickness of a sheet of PCM")
  (subMin lH) Real metre
----------------------
-- Unitless symbols --
----------------------

-- FIXME: this list should not be hand-constructed
unitless :: [DefinedQuantityDict]
unitless = [uNormalVect, surface, eta, meltFrac, gradient, fracMin, consTol,
            aspectRatio, aspectRatioMin, aspectRatioMax]

eta, meltFrac, fracMin, consTol, aspectRatio, aspectRatioMin, aspectRatioMax :: DefinedQuantityDict

-- FIXME: should this have units?
eta = quantNoUnit (mkUid "eta") (nounPhraseSP "ODE parameter related to decay rate")
  (S "derived parameter based on rate of change of temperature of water")
  lEta Real

meltFrac = quantNoUnit (mkUid "meltFrac") (nounPhraseSP "melt fraction")
  (S "ratio of thermal energy to amount of mass melted")
  --FIXME: Not sure if definition is exactly correct
  lPhi Real

fracMin = quantNoUnit (mkUid "fracMin")
  (nounPhraseSP "minimum fraction of the tank volume taken up by the PCM")
  (S "minimum fraction of the tank volume taken up by the PCM")
   (variable "MINFRACT") Real

consTol = quantNoUnit (mkUid "consTol")
  (nounPhraseSP "relative tolerance for conservation of energy")
  (S "relative tolerance for conservation of energy")
  (sub cC lTol) Real

aspectRatio = quantNoUnit (mkUid "aspectRatio")
  (nounPhraseSP "aspect ratio")
  (S "ratio of tank diameter to tank length")
   (variable "AR") Real

aspectRatioMin = quantNoUnit (mkUid "aspectRatioMin")
  (nounPhraseSP "minimum aspect ratio") (S"minimum aspect ratio")
   (subMin (eqSymb aspectRatio)) Real

aspectRatioMax = quantNoUnit (mkUid "aspectRatioMax")
  (nounPhraseSP "maximum aspect ratio") (S "maximum aspect ratio")
   (subMax (eqSymb aspectRatio)) Real

-----------------
-- Constraints --
-----------------

constrained :: [ConstrConcept]
constrained = map cnstrw' inputConstraints ++ map cnstrw' (NE.toList outputs)

-- Input Constraints
inputs :: NE.NonEmpty DefinedQuantityDict
inputs = NE.map dqdWr (absTol :| [relTol]) `NE.appendList` map dqdWr inputConstraints

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
  "the length of the tank" cL metre Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Inc, sy tankLengthMin) (Inc, sy tankLengthMax)] (dbl 1.5)
  defaultUncrt

-- Constraint 2
diam = uqc "diam" (nounPhraseSP "diameter of tank")
  "the diameter of the tank" cD metre Real
  [gtZeroConstr, sfwrRange $ Bounded (Inc, sy arMin) (Inc, sy arMax)]
  (dbl 0.412) defaultUncrt

-- Constraint 3
pcmVol = uqc "pcmVol" (nounPhraseSP "volume of PCM")
  "the amount of space occupied by a given quantity of phase change material"
  (sub (eqSymb vol) lPCM) m_3 Real
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, sy tankVol),
   sfwrRange $ UpFrom (Inc, sy fracMin $* sy tankVol)]
  (dbl 0.05) defaultUncrt
  -- needs to add (D,L)*minfract to end of last constraint

-- Constraint 4
-- Capitalization Issue here too.
pcmSA = uqc "pcmSA"
  (compoundPhrase (nounPhraseSP "phase change material") (surArea ^. term))
  "area covered by the outermost layer of the phase change material"
  (sub cA lPCM) m_2 Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Inc, sy pcmVol) (Inc, (exactDbl 2 $/ sy thickness) $* sy tankVol)]
  (dbl 1.2) defaultUncrt

-- Constraint 5
pcmDensity = uq (cuc'' "pcmDensity" (nounPhraseSP "density of PCM")
  "Mass per unit volume of the phase change material"
  (autoStage $ sub (eqSymb density) lPCM) densityU Real
  [gtZeroConstr, sfwrRange $ Bounded (Exc, sy pcmDensityMin) (Exc, sy pcmDensityMax)]
  (exactDbl 1007)) defaultUncrt

-- Constraint 6
tempMeltP = uqc "tempMeltP"
  (nounPhraseSP "melting point temperature for PCM")
  "temperature at which the phase change material transitions from a solid to a liquid"
  (sup (sub (eqSymb temp) lMelt) lPCM) centigrade Real
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, sy tempC)] (dbl 44.2) defaultUncrt

-- Constraint 7
htCapSP = uqc "htCapSP"
  (nounPhraseSP "specific heat capacity of PCM as a solid")
  ("the amount of energy required to raise the temperature of a " ++
  "given unit mass of solid phase change material by a given amount")
  (sup (sub (eqSymb heatCapSpec) lPCM) lSolid) UT.heatCapSpec Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Exc, sy htCapSPMin) (Exc, sy htCapSPMax)]
  (exactDbl 1760) defaultUncrt

-- Constraint 8
htCapLP = uqc "htCapLP"
  (nounPhraseSP "specific heat capacity of PCM as a liquid")
  ("the amount of energy required to raise the temperature of a " ++
  "given unit mass of liquid phase change material by a given amount")
  (sup (sub (eqSymb heatCapSpec) lPCM) lLiquid) UT.heatCapSpec Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Exc, sy htCapLPMin) (Exc, sy htCapLPMax )]
  (exactDbl 2270) defaultUncrt

--Constraint 9
htFusion = uqc "htFusion" (nounPhraseSP "specific latent heat of fusion")
  "amount of thermal energy required to completely melt a unit mass of a substance"
  (sub cH lFusion) specificE Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Exc, sy htFusionMin) (Exc, sy htFusionMax)] (exactDbl 211600) defaultUncrt

-- Constraint 10
-- The "S "heating coil" " should be replaced by "phrase coil",
-- Since the capitalization issue, they are replaced by S so far.
coilSA = uqc "coilSA"
  (compoundPhrase (nounPhraseSP "heating coil") (surArea ^. term))
  "area covered by the outermost layer of the coil" (sub cA lCoil) m_2 Real
  [gtZeroConstr,
  sfwrRange $ UpTo (Inc, sy coilSAMax)] (dbl 0.12) defaultUncrt

-- Constraint 11
tempC = uqc "tempC" (nounPhraseSP "temperature of the heating coil")
  "the average kinetic energy of the particles within the coil"
  (sub (eqSymb temp) lCoil) centigrade Real
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)] (exactDbl 50) defaultUncrt

-- Constraint 12
wDensity = uq (cuc'' "wDensity" (density `of_` water)
  "mass per unit volume of water" (autoStage $ sub (eqSymb density) lWater) densityU Real
  [gtZeroConstr, sfwrRange $ Bounded (Exc, sy wDensityMin) (Inc, sy wDensityMax)]
  (exactDbl 1000)) defaultUncrt

-- Constraint 13
htCapW = uqc "htCapW" (heatCapSpec `of_` water)
  ("the amount of energy required to raise the " ++
   "temperature of a given unit mass of water by a given amount")
  (sub (eqSymb heatCapSpec) lWater) UT.heatCapSpec Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Exc, sy htCapWMin) (Exc, sy htCapWMax)] (exactDbl 4186) defaultUncrt

-- Constraint 14
coilHTC = uqc "coilHTC" (nounPhraseSP
  "convective heat transfer coefficient between coil and water")
  ("the convective heat transfer coefficient that models " ++
  "the thermal flux from the coil to the surrounding water")
  (sub (eqSymb htTransCoeff) lCoil)
  UT.heatTransferCoef Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Inc, sy coilHTCMin) (Inc, sy coilHTCMax)] (exactDbl 1000) defaultUncrt

-- Constraint 15
pcmHTC = uqc "pcmHTC"
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  ("the convective heat transfer coefficient that models " ++
   "the thermal flux from the phase change material to the surrounding water")
  (sub lH lPCM) UT.heatTransferCoef Real
  [gtZeroConstr,
  sfwrRange $ Bounded (Inc, sy pcmHTCMin) (Inc, sy pcmHTCMax)] (exactDbl 1000) defaultUncrt

-- Constraint 16
tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "the temperature at the beginning of the simulation"
  (sub (eqSymb temp) lInit) centigrade Real
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, sy meltPt)] (exactDbl 40) defaultUncrt

-- Constraint 17
timeFinal = uqc "timeFinal" (nounPhraseSP "final time")
  ("the amount of time elapsed from the beginning of the " ++
   "simulation to its conclusion") (sub (eqSymb time)
  lFinal) second Real
  [gtZeroConstr,
  sfwrRange $ UpTo (Exc, sy timeFinalMax)] (exactDbl 50000) defaultUncrt

timeStep = uqc "timeStep" (nounPhraseSP "time step for simulation")
  ("the finite discretization of time used in the numerical method " ++
   "for solving the computational model")
  (sub (eqSymb time) lStep) second Real
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, sy timeFinal)]
  (dbl 0.01) defaultUncrt

-- Output Constraints
outputs :: NE.NonEmpty ConstrConcept
--FIXME: Add typical values or use Nothing if not known
outputs = tempW :| [tempPCM, watE, pcmE]

-- Constraint 18
tempW = cuc' "tempW"
  (nounPhraseSP "temperature of the water")
  "the average kinetic energy of the particles within the water"
  (sub (eqSymb temp) lWater) centigrade (Vect Real)
  [physRange $ Bounded (Inc, sy tempInit) (Inc, sy tempC)] (exactDbl 0)

-- Constraint 19
tempPCM = cuc' "tempPCM"
  (nounPhraseSP "temperature of the phase change material")
  "the average kinetic energy of the particles within the phase change material"
  (sub (eqSymb temp) lPCM) centigrade Real
  [physRange $ Bounded (Inc, sy tempInit) (Inc, sy tempC)] (exactDbl 0)

-- Constraint 20
watE = cuc' "watE" (nounPhraseSP "change in heat energy in the water")
  "change in thermal energy within the water"
  (sub (eqSymb sensHeat) lWater) joule Real
  [physRange $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)

-- Constraint 21
pcmE = cuc' "pcmE" (nounPhraseSP "change in heat energy in the PCM")
  "change in thermal energy within the phase change material"
  (sub (eqSymb sensHeat) lPCM) joule Real
  [physRange $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)

---------------------------------
-- Uncertainties with no Units --
---------------------------------

absTol, relTol :: UncertQ

absTol = uq (constrained' (quantNoUnit (mkUid "absTol") (nounPhraseSP "absolute tolerance")
  (S "the absolute tolerance") (sub cA lTol) Real)
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 1)]
   (dbl (10.0**(-10)))) (uncty 0.01 Nothing)

relTol = uq (constrained' (quantNoUnit (mkUid "relTol") (nounPhraseSP "relative tolerance")
  (S "the relative tolerance") (sub cR lTol) Real)
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 1)]
  (dbl (10.0**(-10)))) (uncty 0.01 Nothing)

-------------------------
-- Max / Min Variables --
-------------------------

specParamValList :: [ConstQDef]
specParamValList = [tankLengthMin, tankLengthMax, pcmDensityMin, pcmDensityMax,
  wDensityMin, wDensityMax, htCapSPMin, htCapSPMax, htCapLPMin, htCapLPMax,
  htFusionMin, htFusionMax, coilSAMax, htCapWMin, htCapWMax, coilHTCMin,
  coilHTCMax, pcmHTCMin, pcmHTCMax, timeFinalMax, fracMinAux, consTolAux,
  arMin, arMax]

tankLengthMin, tankLengthMax, pcmDensityMin,
  pcmDensityMax, wDensityMin, wDensityMax, htCapSPMin, htCapSPMax, htCapLPMin,
  htCapLPMax, htFusionMin, htFusionMax, coilSAMax, htCapWMin, htCapWMax,
  coilHTCMin, coilHTCMax, pcmHTCMin, pcmHTCMax, timeFinalMax, fracMinAux,
  consTolAux, arMin, arMax :: ConstQDef

consTolAux = mkQuantDef consTol $ perc 1 5

-- Used in Constraint 1
tankLengthMin = mkQuantDef (quant (mkUid "tankLengthMin")
  (nounPhraseSP "minimum length of tank")
  (S "the minimum length of the tank")
  (subMin (eqSymb tankLength)) Real metre) $ dbl 0.1

tankLengthMax = mkQuantDef (quant (mkUid "tankLengthMax")
  (nounPhraseSP "maximum length of tank")
  (S "the maximum length of the tank")
  (subMax (eqSymb tankLength)) Real metre) $ exactDbl 50

fracMinAux = mkQuantDef fracMin $ dbl 1.0e-6

arMin = mkQuantDef aspectRatioMin $ dbl 0.01
arMax = mkQuantDef aspectRatioMax $ exactDbl 100

-- Used in Constraint 5
pcmDensityMin = mkQuantDef (quant' (mkUid "pcmDensityMin")
  (nounPhraseSP "minimum density of PCM")
  (S "the minimum density of the" +:+ phrase phsChgMtrl) (staged (supMin (eqSymb pcmDensity))
  (subMin (unicodeConv $ eqSymb pcmDensity))) Real densityU) $ exactDbl 500

pcmDensityMax = mkQuantDef (quant' (mkUid "pcmDensityMax")
  (nounPhraseSP "maximum density of PCM")
  (S"the maximum density of the" +:+ phrase phsChgMtrl) (staged (supMax (eqSymb pcmDensity))
  (subMax (unicodeConv $ eqSymb pcmDensity))) Real densityU) $ exactDbl 20000

-- Used in Constraint 7
htCapSPMin = mkQuantDef (quant (mkUid "htCapSPMin")
  (nounPhraseSP "minimum specific heat capacity of PCM as a solid")
  (S "the minimum" +:+ phrase heatCapSpec `S.ofThe` phrase phsChgMtrl +:+ S "as a solid")
  (subMin (eqSymb htCapSP)) Real UT.heatCapSpec) $ exactDbl 100

htCapSPMax = mkQuantDef (quant (mkUid "htCapSPMax")
  (nounPhraseSP "maximum specific heat capacity of PCM as a solid")
  (S "the maximum" +:+ phrase heatCapSpec `S.ofThe` phrase phsChgMtrl +:+ S "as a solid")
  (subMax (eqSymb htCapSP)) Real UT.heatCapSpec) $ exactDbl 4000

-- Used in Constraint 8
htCapLPMin = mkQuantDef (quant (mkUid "htCapLPMin")
  (nounPhraseSP "minimum specific heat capacity of PCM as a liquid")
  (S "the minimum" +:+ phrase heatCapSpec `S.ofThe` phrase phsChgMtrl +:+ S "as a liquid")
  (subMin (eqSymb htCapLP)) Real UT.heatCapSpec) $ exactDbl 100

htCapLPMax = mkQuantDef (quant (mkUid "htCapLPMax")
  (nounPhraseSP "maximum specific heat capacity of PCM as a liquid")
  (S "the maximum" +:+ phrase heatCapSpec `S.ofThe` phrase phsChgMtrl +:+ S "as a liquid")
  (subMax (eqSymb htCapLP)) Real UT.heatCapSpec) $ exactDbl 5000

-- Used in Constraint 9
htFusionMin = mkQuantDef (quant (mkUid "htFusionMin")
  (nounPhraseSP "minimum specific latent heat of fusion")
  (S "the minimum specific latent heat of fusion")
  (subMin (eqSymb htFusion)) Real UT.heatCapSpec) $ exactDbl 0

htFusionMax = mkQuantDef (quant (mkUid "htFusionMax")
  (nounPhraseSP "maximum specific latent heat of fusion")
  (S "the maximum specific latent heat of fusion")
  (subMax (eqSymb htFusion)) Real UT.heatCapSpec) $ exactDbl 1000000

-- Used in Constraint 10
coilSAMax = mkQuantDef (quant' (mkUid "coilSAMax")
  (nounPhraseSP "maximum surface area of coil")
  (S "the maximum surface area of the heating coil") (staged (supMax (eqSymb coilSA))
  (subMax (eqSymb coilSA))) Real m_2) $ exactDbl 100000

-- Used in Constraint 12
wDensityMin = mkQuantDef (quant' (mkUid "wDensityMin")
  (nounPhraseSP "minimum density of water")
  (S "the minimum density of water") (staged (supMin (eqSymb wDensity))
  (subMin (unicodeConv $ eqSymb wDensity))) Real densityU) $ exactDbl 950

wDensityMax = mkQuantDef (quant' (mkUid "wDensityMax")
  (nounPhraseSP "maximum density of water")
  (S "the maximum density of water") (staged (supMax (eqSymb wDensity))
  (subMax (unicodeConv $ eqSymb wDensity))) Real densityU) $ exactDbl 1000

-- Used in Constraint 13
htCapWMin = mkQuantDef (quant' (mkUid "htCapWMin")
  (nounPhraseSP "minimum specific heat capacity of water")
  (S "the minimum" +:+ phrase heatCapSpec +:+ S "of water") (staged (supMin (eqSymb htCapW))
  (subMin (eqSymb htCapW))) Real UT.heatCapSpec) $ exactDbl 4170

htCapWMax = mkQuantDef (quant' (mkUid "htCapWMax")
  (nounPhraseSP "maximum specific heat capacity of water")
  (S "the maximum" +:+ phrase heatCapSpec +:+ S "of water") (staged (supMax (eqSymb htCapW))
  (subMax (eqSymb htCapW))) Real UT.heatCapSpec) $ exactDbl 4210

-- Used in Constraint 14
coilHTCMin = mkQuantDef (quant' (mkUid "coilHTCMin")
  (nounPhraseSP $ "minimum convective heat " ++
  "transfer coefficient between coil and water")
  (S "the minimum convective heat transfer coefficient between the coil and water")
  (staged (supMin (eqSymb coilHTC)) (subMin (eqSymb coilHTC)))
  Real UT.heatTransferCoef) $ exactDbl 10

coilHTCMax = mkQuantDef (quant' (mkUid "coilHTCMax")
  (nounPhraseSP $ "maximum convective heat " ++
  "transfer coefficient between coil and water")
  (S "the maximum convective heat transfer coefficient between the coil and water")
  (staged (supMax (eqSymb coilHTC)) (subMax (eqSymb coilHTC)))
  Real UT.heatTransferCoef) $ exactDbl 10000

-- Used in Constraint 15
pcmHTCMin = mkQuantDef (quant' (mkUid "pcmHTCMin")
  (nounPhraseSP $ "minimum convective heat " ++
  "transfer coefficient between PCM and water")
  (S "the minimum convective heat transfer coefficient between the" +:+
  phrase phsChgMtrl +:+ S "and water")
  (staged (supMin (eqSymb pcmHTC)) (subMin (eqSymb pcmHTC)))
  Real UT.heatTransferCoef) $ exactDbl 10

pcmHTCMax = mkQuantDef (quant' (mkUid "pcmHTCMax")
  (nounPhraseSP $ "maximum convective heat " ++
  "transfer coefficient between PCM and water")
  (S "the maximum convective heat transfer coefficient between the" +:+
  phrase phsChgMtrl +:+ S "and water")
  (staged (supMax (eqSymb pcmHTC)) (subMax (eqSymb pcmHTC)))
  Real UT.heatTransferCoef) $ exactDbl 10000

-- Used in Constraint 17
timeFinalMax = mkQuantDef (quant' (mkUid "timeFinalMax")
  (nounPhraseSP "maximum final time")
  (S "the maximum final time for the simulation")
  (staged (supMax (eqSymb timeFinal)) (subMax (eqSymb timeFinal)))
  Real second) $ exactDbl 86400

-- Labels
lCoil, lEnv, lFinal, lFusion, lIn, lInit, lLiquid, lMelt, lOut, lPCM, lSolid,
  lStep, lTank, lTol, lVapour, lWater :: Symbol
lCoil   = label "C"
lEnv    = label "env"
lFinal  = label "final"
lFusion = label "f"
lIn     = label "in"
lInit   = label "init"
lLiquid = label "L"
lMelt   = label "melt"
lOut    = label "out"
lPCM    = label "P"
lSolid  = label "S"
lStep   = label "step"
lTank   = label "tank"
lTol    = label "tol"
lVapour = label "V"
lWater  = label "W"
