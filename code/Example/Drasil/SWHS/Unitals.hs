module Drasil.SWHS.Unitals where --all of this file is exorted

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.SI_Units (m_2, second, kilogram, metre, joule,
  centigrade, m_3, specificE)
import Data.Drasil.Concepts.Documentation (simulation)
import qualified Data.Drasil.Units.Thermodynamics as UT (heat_transfer_coef,
  heat_cap_spec, thermal_flux, volHtGenU)
import Data.Drasil.Quantities.Thermodynamics (sens_heat, temp, melt_pt,
  ht_flux, latent_heat, boil_pt, heat_cap_spec)
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.Math (surface, uNormalVect, surArea)
import Data.Drasil.Quantities.PhysicalProperties (mass, density, vol)
import Drasil.SWHS.Concepts (water, coil, phsChgMtrl)
import Data.Drasil.Units.PhysicalProperties (densityU)
import Data.Drasil.Utils(mkDataDef)
import Data.Drasil.Quantities.Math (gradient)

swhsSymbols :: [CQSWrapper]
swhsSymbols = (map cqs swhsUnits) ++ (map cqs swhsUnitless) ++
  (map cqs swhsConstrained)

swhsSymbolsAll :: [QWrapper] -- UCWrapper, ConVar, UncertQ, QDefinition, UnitaryChunk, 
swhsSymbolsAll = (map qs swhsUnits) ++ (map qs swhsUnitless) ++
  (map qs swhsConstrained) ++ (map qs specParamValList) ++
  (map qs [htFusion_min, htFusion_max, coil_SA_max] ++ [qs gradient])

-- Symbols with Units --

swhsUnits :: [UCWrapper]
swhsUnits = map ucw [in_SA, out_SA, heat_cap_spec, htCap_L,
  htCap_S, htCap_V, sens_heat, pcm_initMltE,
  vol_ht_gen, htTransCoeff, pcm_mass, w_mass, ht_flux, latent_heat,
  thFluxVect, ht_flux_C, ht_flux_in, ht_flux_out, ht_flux_P, latentE_P,
  temp,boil_pt, temp_env, melt_pt, t_init_melt,
  t_final_melt, vol, tank_vol, w_vol, deltaT,
  density, tau, tau_L_P, tau_S_P, tau_W] ++
  map ucw [mass, time] -- ++ [tank_length, diam, coil_SA]

in_SA, out_SA, htCap_L, htCap_S, htCap_V,
  pcm_initMltE, vol_ht_gen, htTransCoeff,
  pcm_mass, w_mass,
  thFluxVect, ht_flux_C, ht_flux_in, ht_flux_out, ht_flux_P, latentE_P,
  temp_env, t_init_melt,
  t_final_melt, tank_vol, w_vol, deltaT,
  tau, tau_L_P, tau_S_P, tau_W, sim_time :: UnitalChunk

---------------------
-- Regular Symbols --
---------------------

--symbol names can't begin with a capital

in_SA = uc' "in_SA" (nounPhraseSP
  "surface area over which heat is transferred in")
  "Surface area over which thermal energy is transferred into an object"
  (sub cA (Atomic "in")) m_2

out_SA = uc' "out_SA" (nounPhraseSP
  "surface area over which heat is transferred out")
  "Surface area over which thermal energy is transferred out of an object"
  (sub cA (Atomic "out")) m_2

htCap_L = uc' "htCap_L" (nounPhraseSP "specific heat capacity of a liquid")
  ("The amount of energy required to raise the temperature of a given " ++
  "unit mass of a given liquid by a given amount")
  (sup (eqSymb heat_cap_spec) cL) UT.heat_cap_spec

htCap_S = uc' "htCap_S"
  (nounPhraseSP "specific heat capacity of a solid")
  ("The amount of energy required to raise the temperature of " ++
  "a given unit mass of a given solid by a given amount")
  (sup (eqSymb heat_cap_spec) cS) UT.heat_cap_spec

htCap_V = uc' "htCap_V"
  (nounPhraseSP "specific heat capacity of a vapour")
  ("The amount of energy required to raise the temperature of a given " ++
  "unit mass of vapour by a given amount")
  (sup (eqSymb heat_cap_spec) cV) UT.heat_cap_spec

pcm_initMltE = uc' "pcm_initMltE" (nounPhraseSP
  "change in heat energy in the PCM at the instant when melting begins")
  "Change in thermal energy in the phase change material at the melting point"
  (sup (sub (sub (eqSymb sens_heat)
  (Atomic "P")) (Atomic "melt")) (Atomic "init")) joule

vol_ht_gen = uc' "vol_ht_gen"
  (nounPhraseSP "volumetric heat generation per unit volume")
  "Amount of thermal energy generated per unit volume" lG UT.volHtGenU

htTransCoeff = uc' "htTransCoeff"
  (nounPhraseSP "convective heat transfer coefficient")
  ("The proportionality constant between the heat flux and the " ++
  "thermodynamic driving force for the flow of thermal energy")
  lH UT.heat_transfer_coef

pcm_mass = uc' "pcm_mass" (nounPhraseSP "mass of phase change material")
  "The quantity of matter within the phase change material"
  (sub (eqSymb mass) cP) kilogram

w_mass = uc' "w_mass" (nounPhraseSP "mass of water")
  "The quantity of matter within the water" (sub (eqSymb mass) cW) kilogram

thFluxVect = uc' "thFluxVect" (nounPhraseSP "thermal flux vector")
  "Vector denoting the direction of thermal flux through a surface"
  (vec lQ) UT.thermal_flux

ht_flux_C = uc' "ht_flux_C"
  (nounPhraseSP "heat flux into the water from the coil")
  "The rate of heat energy transfer into the water from the coil per unit time"
  (sub (eqSymb ht_flux) cC) UT.thermal_flux

ht_flux_in = uc' "ht_flux_in" (nounPhraseSP "heat flux input")
  "The rate of heat energy transfer into an object per unit time"
  (sub (eqSymb ht_flux) (Atomic "in")) UT.thermal_flux

ht_flux_out = uc' "ht_flux_out" (nounPhraseSP "heat flux output")
  "The rate of heat energy transfer into an object per unit time"
  (sub (eqSymb ht_flux) (Atomic "out")) UT.thermal_flux

ht_flux_P = uc' "ht_flux_P" (nounPhraseSP "heat flux into the PCM from water")
  ("The rate of heat energy transfer into the phase" ++
  "change material from the water per unit time")
  (sub (eqSymb ht_flux) cP) UT.thermal_flux

latentE_P = uc' "latentE_P" (nounPhraseSP "latent heat energy added to PCM")
  ("Energy released or absorbed, by a body or a thermodynamic system, "++
  "during a constant-temperature process and absorbed by the phase" ++
  "change material") (sub (eqSymb latent_heat) cP) joule

temp_env = uc' "temp_env" (nounPhraseSP "temperature of the environment")
  "The tempature of a given environment"
  (sub (eqSymb temp) (Atomic "env")) centigrade

t_init_melt = uc' "t_init_melt"
  (nounPhraseSP "time at which melting of PCM begins")
  ("Time at which the phase change material " ++
    "begins changing from a solid to a liquid")
  (sup (sub (eqSymb time) (Atomic "melt")) (Atomic "init")) second

t_final_melt = uc' "t_final_melt"
  (nounPhraseSP "time at which melting of PCM ends")
  ("Time at which the phase change material " ++
    "finishes changes from a solid to a liquid")
  (sup (sub (eqSymb time) (Atomic "melt")) (Atomic "final")) second
  
tank_vol = uc' "tank_vol" (nounPhraseSP "volume of the cylindrical tank")
  "The amount of space encompassed by a tank"
  (sub (eqSymb vol) (Atomic "tank")) m_3

w_vol = uc' "w_vol" (vol `of_` water)
  "The amount of space occupied by a given quantity of water"
  (sub (eqSymb vol) cW) m_3

deltaT = uc' "deltaT" (nounPhraseSP "change in temperature")
  "Change in the average kinetic energy of a given material"
  (Concat [Greek Delta, (eqSymb temp)]) centigrade

tau = uc' "tau" (nounPhraseSP "dummy variable for integration over time")
  "Binary value representing the presence or absence of integration over time"
  (Greek Tau_L) second
--Not sure how to define anything after this point

tau_L_P = uc' "tau_L_P" (nounPhraseSP "ODE parameter for liquid PCM")
  ("Derived through melting of phase change material, which " ++
  "changes ODE parameter for solid PCM into parameter for liquid")
  (sup (sub (Greek Tau_L) cP) cL) second

tau_S_P = uc' "tau_S_P" (nounPhraseSP "ODE parameter for solid PCM")
  ("Derived parameter based on rate of " ++
    "change of temperature of phase change material")
  (sup (sub (Greek Tau_L) cP) cS) second

tau_W = uc' "tau_W" (nounPhraseSP "ODE parameter for water")
  "Derived parameter based on rate of change of temperature of water"
  (sub (Greek Tau_L) cW) second

sim_time = uc' "sim_time" (compoundPhrase' (simulation ^. term)
  (time ^. term)) "Time over which the simulation runs"
  lT second

----------------------
-- Unitless symbols --
----------------------
swhsUnitless :: [ConVar]
swhsUnitless = [uNormalVect, surface, eta, melt_frac, frac_min]

eta, melt_frac, frac_min :: ConVar

eta = cvR (dcc "eta" (nounPhraseSP "ODE parameter")
  "Derived parameter based on rate of change of temperature of water")
  (Greek Eta_L)

melt_frac = cvR (dcc "melt_frac" (nounPhraseSP "melt fraction")
  "Ratio of thermal energy to amount of mass melted")
  --FIXME: Not sure if definition is exactly correct
  (Greek Phi_L)

frac_min = cvR (dcc "frac_min" (nounPhraseSP "minmum fraction")
  "minmum fraction")
  (Atomic "MINFRACT")


-----------------
-- Constraints --
-----------------

swhsConstrained ::[UncertQ]
swhsConstrained = swhsInputs ++ swhsOutputs

-- Input Constraints
swhsInputs :: [UncertQ]
swhsInputs = [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C,
  w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final]

tank_length, diam, pcm_vol, pcm_SA, pcm_density, temp_melt_P,
  htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C, w_density,
  htCap_W, coil_HTC, pcm_HTC, temp_init, time_final, temp_PCM, 
  temp_W, w_E, pcm_E :: UncertQ

-- Constraint 1
tank_length = uqc "tank_length" (nounPhraseSP "length of tank")
  "The length of the tank" cL metre Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C tank_length_min $<= c $<= C tank_length_max] (Dbl 1.5)
  0.1

-- Constraint 2
diam = uqc "diam" (nounPhraseSP "diameter of tank")
  "The diameter of the tank" cD metre Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> (c / C tank_length_max) $<=
  (c / C tank_length) $<= (c / C tank_length_min)] (Dbl 0.412) 0.1

-- Constraint 3
pcm_vol = uqc "pcm_vol" (nounPhraseSP "volume of PCM")
  "The amount of space occupied by a given quantity of phase change material"
  (sub (eqSymb vol) cP) m_3 Rational
  [physc $ \c -> c $> Int 0,
  physc $ \c -> c $< C tank_vol,
  sfwrc $ \c -> c $>= (C frac_min) * (C tank_vol)] (Dbl 0.05) 0.1
  -- needs to add (D,L)*minfract to end of last constraint

-- Constraint 4
pcm_SA = uqc "pcm_SA"
  (compoundPhrase (nounPhrase'' (phrase phsChgMtrl) (phrase phsChgMtrl)
  CapFirst CapWords) (nounPhrase'' (phrase surArea) (phrase surArea)
  CapFirst CapWords))
  "Area covered by the outermost layer of the phase change material"
  (sub cA cP) m_2 Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C pcm_vol $<= c $<=
  ((Int 2 / C htTransCoeff_min) * C tank_vol)]
  (Dbl 1.2) 0.1

-- Constraint 5
pcm_density = uqc "pcm_density" (nounPhraseSP "density of PCM")
  "Mass per unit volume of the phase change material"
  (sub (eqSymb density) cP) densityU Rational
  [physc $ \c -> c $> Int 0,
  physc $ \c -> C pcm_density_min $< c $< C pcm_density_max] (Dbl 1007) 0.1

-- Constraint 6
temp_melt_P = uqc "temp_melt_P"
  (nounPhraseSP "melting point temperature for PCM")
  ("Temperature at which the phase change " ++
    "material transitions from a solid to a liquid")
  (sup (sub (eqSymb temp) (Atomic "melt")) cP) centigrade Rational
  [physc $ \c -> Int 0 $< c $< C temp_C] (Dbl 44.2) 0.1

-- Constraint 7
htCap_S_P = uqc "htCap_S_P"
  (nounPhraseSP "specific heat capacity of PCM as a solid")
  ("The amount of energy required to raise the temperature of a " ++
  "given unit mass of solid phase change material by a given amount")
  (sup (sub (eqSymb heat_cap_spec) cP) cS) UT.heat_cap_spec Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> (Index (c) (V "min")) $< c $< (Index (c) (V "max"))]
  (Dbl 1760) 0.1

-- Constraint 8
htCap_L_P = uqc "htCap_L_P"
  (nounPhraseSP "specific heat capacity of PCM as a liquid")
  ("The amount of energy required to raise the temperature of a " ++
  "given unit mass of liquid phase change material by a given amount")
  (sup (sub (eqSymb heat_cap_spec) cP) cL) UT.heat_cap_spec Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> (Index (c) (V "min")) $< c $< (Index (c) (V "max"))]
  (Dbl 2270) 0.1

--Constraint 9
htFusion = uqc "htFusion" (nounPhraseSP "specific latent heat of fusion")
  ("amount of thermal energy required to " ++
  "completely melt a unit mass of a substance")
  (sub cH lF) specificE Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C htFusion_min $< c $< C htFusion_max] (Dbl 211600) 0.1

-- Constraint 10
coil_SA = uqc "coil_SA"
  (compoundPhrase (nounPhrase'' (phrase coil) (phrase coil) CapFirst CapWords)
  (nounPhrase'' (phrase surArea) (phrase surArea) CapFirst CapWords))
  "Area covered by the outermost layer of the coil" (sub cA cC) m_2 Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> c $<= C coil_SA_max] (Dbl 0.12) 0.1

-- Constraint 11
temp_C = uqc "temp_C" (nounPhraseSP "temperature of the heating coil")
  "The average kinetic energy of the particles within the coil"
  (sub (eqSymb temp) cC) centigrade Rational
  [physc $ \c -> Int 0 $< c $< Int 100] (Dbl 50) 0.1

-- Constraint 12
w_density = uqc "w_density" (density `of_` water)
  "Mass per unit volume of water"
  (sub (eqSymb density) cW) densityU Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C w_density_min $< c $<= C w_density_max] (Dbl 1000) 0.1
  
-- Constraint 13
htCap_W = uqc "htCap_W" (heat_cap_spec `of_` water)
  ("The amount of energy required to raise the " ++
    "temperature of a given unit mass of water by a given amount")
  (sub (eqSymb heat_cap_spec) cW) UT.heat_cap_spec Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C htCap_W_min $< c $< C htCap_W_max] (Dbl 4186) 0.1
  
-- Constraint 14
coil_HTC = uqc "coil_HTC" (nounPhraseSP
  "convective heat transfer coefficient between coil and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the coil to the surrounding water")
  (sub (eqSymb htTransCoeff) cC)
  UT.heat_transfer_coef Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C coil_HTC_min $<= c $<= C coil_HTC_max] (Dbl 1000) 0.1
  
-- Constraint 15
pcm_HTC = uqc "pcm_HTC"
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the phase change material to the surrounding water")
  (sub lH cP) UT.heat_transfer_coef Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> C pcm_HTC_min $<= c $<= C pcm_HTC_max] (Dbl 1000) 0.1
  
-- Constraint 16
temp_init = uqc "temp_init" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (eqSymb temp)(Atomic "init")) centigrade Rational
  [physc $ \c -> 0 $< c $< C melt_pt] (Dbl 40) 0.1
  
-- Constraint 17
time_final = uqc "time_final" (nounPhraseSP "final time")
  ("The amount of time elapsed from the beginning of the " ++
  "simulation to its conclusion") (sub (eqSymb time) 
  (Atomic "final")) second Rational
  [physc $ \c -> c $> Int 0,
  sfwrc $ \c -> c $< C time_final_max] (Dbl 50000) 0.1
  
  
-- Output Constraints
swhsOutputs :: [UncertQ]
--FIXME: Add typical values or use Nothing if not known
swhsOutputs = [temp_W, temp_PCM, w_E, pcm_E]

-- Constraint 18
temp_W = uqcNU "temp_W"
  (nounPhraseSP "temperature of the water")
  "The average kinetic energy of the particles within the water" 
  (sub (eqSymb temp) cW) centigrade Rational
  [physc $ \c -> C temp_init $<= c $<= C temp_C] (Dbl 0)

-- Constraint 19
temp_PCM = uqcNU "temp_PCM"
  (nounPhraseSP "temperature of the phase change material" )
  ("The average kinetic energy of the " ++
    "particles within the phase change material")
  (sub (eqSymb temp) cP) centigrade Rational
  [physc $ \c -> C temp_init $<= c $<= C temp_C] (Dbl 0)
  
-- Constraint 20
w_E = uqcNU "w_E" (nounPhraseSP "change in heat energy in the water")
  "Change in thermal energy within the water" 
  (sub (eqSymb sens_heat) cW) joule Rational
  [physc $ \c -> c $>= Int 0] (Dbl 0)
  
-- Constraint 21
pcm_E = uqcNU "pcm_E" (nounPhraseSP "change in heat energy in the PCM")
  "Change in thermal energy within the phase change material" 
  (sub (eqSymb sens_heat) cP) joule Rational
  [physc $ \c -> c $>= Int 0] (Dbl 0)



------------------------------
-- Uncertainties with no Units
------------------------------

abs_tol, rel_tol, cons_tol :: UncertainChunk

abs_tol = uvc "abs_tol" (nounPhraseSP "absolute tolerance") 
  (sub cA (Atomic "tol")) Real
  [ physc $ \c -> (Dbl 0) $< c ,
    physc $ \c -> c $< (Dbl 1) ] (Dbl (10.0**(-10))) 0.01

rel_tol = uvc "pb_tol" (nounPhraseSP "relative tolerance") 
  (sub cR (Atomic "tol")) Real
  [ physc $ \c -> (Dbl 0) $< c ,
    physc $ \c -> c $< (Dbl 1) ] (Dbl (10.0**(-10))) 0.01

cons_tol = uvc "pb_tol"
  (nounPhraseSP "relative tolerance for conservation of energy") 
  (sub cC (Atomic "tol")) Real
  [ physc $ \c -> (Dbl 0) $< c ,
    physc $ \c -> c $< (Dbl 1) ] (Dbl (10.0**(-3))) 0.01



-------------------------
-- Max / Min Variables --
-------------------------

specParamValList :: [QDefinition]
specParamValList = [tank_length_min, tank_length_max,
  htTransCoeff_min, pcm_density_min, pcm_density_max, w_density_min, 
  w_density_max, htCap_S_P_min, htCap_S_P_max, htCap_L_P_min,
  htCap_L_P_max, htCap_W_min, htCap_W_max, coil_HTC_min, 
  coil_HTC_max, pcm_HTC_min, pcm_HTC_max, time_final_max]

tank_length_min, tank_length_max, htTransCoeff_min, pcm_density_min, 
  pcm_density_max, w_density_min, w_density_max, htCap_S_P_min, 
  htCap_S_P_max, htCap_L_P_min, htCap_L_P_max,
  htCap_W_min, htCap_W_max, coil_HTC_min, coil_HTC_max, pcm_HTC_min,
  pcm_HTC_max, time_final_max :: QDefinition

htFusion_min, htFusion_max, coil_SA_max :: UnitaryChunk

-- Used in Constraint 1
tank_length_min = mkDataDef (unitary "tank_length_min"
  (nounPhraseSP "minimum length of tank")
  (sub (eqSymb tank_length) (Atomic "min")) metre Rational) (Dbl 0.1)

tank_length_max = mkDataDef (unitary "tank_length_max"
  (nounPhraseSP "maximum length of tank")
  (sub (eqSymb tank_length) (Atomic "max")) metre Rational) (Int 50)

-- Used in Constraint 4
htTransCoeff_min = mkDataDef (unitary "htTransCoeff_min"
  (nounPhraseSP "minimum convective heat transfer coefficient")
  (sub (eqSymb htTransCoeff) (Atomic "min")) UT.heat_transfer_coef Rational)
  (Dbl 0.001)

-- Used in Constraint 5
pcm_density_min = mkDataDef (unitary "pcm_density_min"
  (nounPhraseSP "minimum density of PCM")
  (sup (eqSymb pcm_density) (Atomic "min")) densityU Rational) (Int 500)

pcm_density_max = mkDataDef (unitary "pcm_density_max"
  (nounPhraseSP "maximum density of PCM")
  (sup (eqSymb pcm_density) (Atomic "max")) densityU Rational) (Int 20000)

-- Used in Constraint 7
htCap_S_P_min = mkDataDef (unitary "htCap_S_P_min"
  (nounPhraseSP "minimum specific heat capacity of PCM as a solid")
  (sub (eqSymb htCap_S_P) (Atomic "min")) UT.heat_cap_spec Rational)
  (Int 100)

htCap_S_P_max = mkDataDef (unitary "htCap_S_P_max"
  (nounPhraseSP "maximum specific heat capacity of PCM as a solid")
  (sub (eqSymb htCap_S_P) (Atomic "max")) UT.heat_cap_spec Rational)
  (Int 4000)

-- Used in Constraint 8
htCap_L_P_min = mkDataDef (unitary "htCap_L_P_min"
  (nounPhraseSP "minimum specific heat capacity of PCM as a liquid")
  (sub (eqSymb htCap_L_P) (Atomic "min")) UT.heat_cap_spec Rational)
  (Int 100)

htCap_L_P_max = mkDataDef (unitary "htCap_L_P_max"
  (nounPhraseSP "maximum specific heat capacity of PCM as a liquid")
  (sub (eqSymb htCap_L_P) (Atomic "max")) UT.heat_cap_spec Rational)
  (Int 5000)

-- Used in Constraint 9
htFusion_min = unitary "htFusion_min"
  (nounPhraseSP "minimum specific latent heat of fusion")
  (sub (eqSymb htFusion) (Atomic "min")) UT.heat_cap_spec Rational

htFusion_max = unitary "htFusion_max"
  (nounPhraseSP "maximum specific latent heat of fusion")
  (sub (eqSymb htFusion) (Atomic "max")) UT.heat_cap_spec Rational

-- Used in Constraint 10
coil_SA_max = unitary "coil_SA_max"
  (nounPhraseSP "maximum surface area of coil")
  (sup (eqSymb coil_SA) (Atomic "max")) m_2 Rational

-- Used in Constraint 12
w_density_min = mkDataDef (unitary "w_density_min"
  (nounPhraseSP "minimum density of water")
  (sup (eqSymb w_density) (Atomic "min")) densityU Rational) (Int 950)

w_density_max = mkDataDef (unitary "w_density_max"
  (nounPhraseSP "maximum density of water")
  (sup (eqSymb w_density) (Atomic "max")) densityU Rational) (Int 1000)
  
-- Used in Constraint 13
htCap_W_min = mkDataDef (unitary "htCap_W_min"
  (nounPhraseSP "minimum specific heat capacity of water")
  (sup (eqSymb htCap_W) (Atomic "min")) UT.heat_cap_spec Rational)
  (Int 4170)

htCap_W_max = mkDataDef (unitary "htCap_W_max"
  (nounPhraseSP "maximum specific heat capacity of water")
  (sup (eqSymb htCap_W) (Atomic "max")) UT.heat_cap_spec Rational)
  (Int 4210)

-- Used in Constraint 14
coil_HTC_min = mkDataDef (unitary "coil_HTC_min"
  (nounPhraseSP $ "minimum convective heat " ++
  "transfer coefficient between coil and water")
  (sup (eqSymb coil_HTC) (Atomic "min")) UT.heat_transfer_coef Rational)
  (Int 10)

coil_HTC_max = mkDataDef (unitary "coil_HTC_max"
  (nounPhraseSP $ "maximum convective heat " ++
  "transfer coefficient between coil and water")
  (sup (eqSymb coil_HTC) (Atomic "max")) UT.heat_transfer_coef Rational)
  (Int 10000)
  
-- Used in Constraint 15
pcm_HTC_min = mkDataDef (unitary "pcm_HTC_min"
  (nounPhraseSP $ "minimum convective heat " ++
  "transfer coefficient between PCM and water")
  (sup (eqSymb pcm_HTC) (Atomic "min")) UT.heat_transfer_coef Rational)
  (Int 10)

pcm_HTC_max = mkDataDef (unitary "pcm_HTC_max"
  (nounPhraseSP $ "maximum convective heat " ++
  "transfer coefficient between PCM and water")
  (sup (eqSymb pcm_HTC) (Atomic "max")) UT.heat_transfer_coef Rational)
  (Int 10000)
  
-- Used in Constraint 17
time_final_max = mkDataDef (unitary "time_final_max"
  (nounPhraseSP "maximum final time")
  (sup (eqSymb time_final) (Atomic "max")) second Rational) (Int 86400)
