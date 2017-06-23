module Drasil.SWHS.Unitals where

import Drasil.SWHS.Concepts

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as UT
import Data.Drasil.Concepts.Thermodynamics (thermal_energy)
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.Math (surface, uNormalVect, surArea)
import Data.Drasil.Quantities.PhysicalProperties (mass, density, vol)
import Data.Drasil.Units.PhysicalProperties

import Control.Lens ((^.))
import Prelude hiding (id)

swhsSymbols :: [CQSWrapper]
swhsSymbols = (map cqs swhsUnits) ++ (map cqs swhsUnitless) ++ (map cqs swhsConstrained)

-- Symbols with Units --

swhsUnits :: [UCWrapper]
swhsUnits = map ucw [in_SA, out_SA, heat_cap_spec, htCap_L,
  htCap_S, htCap_V, sens_heat, pcm_initMltE,
  vol_ht_gen, htTransCoeff, pcm_mass, w_mass, ht_flux, latent_heat,
  thFluxVect, ht_flux_C, ht_flux_in, ht_flux_out, ht_flux_P, latentE_P, temp,
  boil_pt, temp_env, melt_pt, t_init_melt,
  t_final_melt, temp_diff, vol, tank_vol, w_vol, deltaT,
  density, tau, tau_L_P, tau_S_P, tau_W] ++
  map ucw [mass, time] -- ++ [tank_length, diam, coil_SA]

in_SA, out_SA, htCap_L, htCap_S, htCap_V,
  pcm_initMltE, vol_ht_gen, htTransCoeff,
  pcm_mass, w_mass,
  thFluxVect, ht_flux_C, ht_flux_in, ht_flux_out, ht_flux_P, latentE_P,
  temp_env, t_init_melt,
  t_final_melt, temp_diff, tank_vol, w_vol, deltaT,
  tau, tau_L_P, tau_S_P, tau_W :: UnitalChunk


---------------------
-- Regular Symbols --
---------------------

--symbol names can't begin with a capital

in_SA        = uc' "in_SA" (nounPhraseSP
  "surface area over which heat is transferred in")
  "Surface area over which thermal energy is transferred into an object"
  (sub cA (Atomic "in")) m_2

out_SA       = uc' "out_SA" (nounPhraseSP
  "surface area over which heat is transferred out")
  "Surface area over which thermal energy is transferred out of an object"
  (sub cA (Atomic "out")) m_2

htCap_L      = uc' "htCap_L" (nounPhraseSP "specific heat capacity of a liquid")
  ("The amount of energy required to raise the temperature of a given unit mass of " ++
  "a given liquid by a given amount") (sup (heat_cap_spec ^. symbol) cL) UT.heat_cap_spec

htCap_S      = uc' "htCap_S"
  (nounPhraseSP "specific heat capacity of a solid")
  ("The amount of energy required to raise the temperature of " ++
  "a given unit mass of a given solid by a given amount")
  (sup (heat_cap_spec ^. symbol) cS) UT.heat_cap_spec

htCap_V      = uc' "htCap_V"
  (nounPhraseSP "specific heat capacity of a vapour")
  "The amount of energy required to raise the temperature of a given unit mass of vapour by a given amount"
  (sup (heat_cap_spec ^. symbol) cV) UT.heat_cap_spec

pcm_initMltE = uc' "pcm_initMltE" (nounPhraseSP
  "change in heat energy in the PCM at the instant when melting begins")
  "Change in thermal energy in the phase change material at the melting point"
  (sup (sub (sens_heat ^. symbol) (Atomic "Pmelt")) (Atomic "init")) joule

vol_ht_gen   = uc' "vol_ht_gen"
  (nounPhraseSP "volumetric heat generation per unit volume")
  "Amount of thermal energy generated per unit volume" lG UT.volHtGenU

htTransCoeff = uc' "htTransCoeff"
  (nounPhraseSP "convective heat transfer coefficient")
  ("The proportionality constant between the heat flux and the " ++
  "thermodynamic driving force for the flow of thermal energy")
  lH UT.heat_transfer_coef

pcm_mass     = uc' "pcm_mass" (nounPhraseSP "mass of phase change material")
  "The quantity of matter within the phase change material"
  (sub (mass ^. symbol) cP) kilogram

w_mass       = uc' "w_mass" (nounPhraseSP "mass of water")
  "The quantity of matter within the water" (sub (mass ^. symbol) cW) kilogram

thFluxVect   = uc' "thFluxVect" (nounPhraseSP "thermal flux vector")
  "Vector denoting the direction of thermal flux through a surface"
  (vec lQ) UT.thermal_flux

ht_flux_C    = uc' "ht_flux_C"
  (nounPhraseSP "heat flux into the water from the coil")
  "The rate of heat energy transfer into the water from the coil per unit time"
  (sub (ht_flux ^. symbol) cC) UT.thermal_flux

ht_flux_in   = uc' "ht_flux_in" (nounPhraseSP "heat flux input")
  "The rate of heat energy transfer into an object per unit time"
  (sub (ht_flux ^. symbol) (Atomic "in")) UT.thermal_flux

ht_flux_out  = uc' "ht_flux_out" (nounPhraseSP "heat flux output")
  "The rate of heat energy transfer into an object per unit time"
  (sub (ht_flux ^. symbol) (Atomic "out")) UT.thermal_flux

ht_flux_P    = uc' "ht_flux_P" (nounPhraseSP "heat flux into the PCM from water")
  "The rate of heat energy transfer into the phase change material from the water per unit time"
  (sub (ht_flux ^. symbol) cP) UT.thermal_flux

latentE_P    = uc' "latentE_P" (nounPhraseSP "latent heat energy added to PCM")
  ("Energy released or absorbed, by a body or a thermodynamic system, during a constant-temperature " ++
  "process and absorbed by the phase change material") (sub (latent_heat ^. symbol) cP) joule

temp_env     = uc' "temp_env" (nounPhraseSP "temperature of the environment")
  "The tempature of a given environment" (sub (temp ^. symbol) (Atomic "env")) centigrade

t_init_melt  = uc' "t_init_melt"
  (nounPhraseSP "time at which melting of PCM begins")
  "Time at which the phase change material begins changing from a solid to a liquid"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "init")) second

t_final_melt = uc' "t_final_melt"
  (nounPhraseSP "time at which melting of PCM ends")
  "Time at which the phase change material finishes changes from a solid to a liquid"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "final")) second
  
temp_diff   = uc' "temp_diff" (nounPhraseSP "temperature difference")
  "Measure of the relative amounts of internal energy within two bodies" 
  (Concat [Greek Delta, cT]) centigrade
  
tank_vol     = uc' "tank_vol" (nounPhraseSP "volume of the cylindrical tank")
  "The amount of space encompassed by a tank" (sub (vol ^. symbol) (Atomic "tank")) m_3

w_vol        = uc' "w_vol" (vol `of_` water)
  "The amount of space occupied by a given quantity of water" (sub (vol ^. symbol) cW) m_3

deltaT       = uc' "deltaT" (nounPhraseSP "change in temperature")
  "Change in the average kinetic energy of a given material"
  (Concat [Greek Delta, (temp ^. symbol)]) centigrade

tau          = uc' "tau" (nounPhraseSP "dummy variable for integration over time")
  "Binary value representing the presence or absence of integration over time" (Greek Tau_L) second
--Not sure how to define anything after this point

tau_L_P      = uc' "tau_L_P" (nounPhraseSP "ODE parameter for liquid PCM")
  ("Derived through melting of phase change material, which changes ODE parameter " ++
  "for solid PCM into parameter for liquid")
  (sup (sub (Greek Tau_L) cP) cL) second

tau_S_P      = uc' "tau_S_P" (nounPhraseSP "ODE parameter for solid PCM")
  "Derived parameter based on rate of change of temperature of phase change material"
  (sup (sub (Greek Tau_L) cP) cS) second

tau_W        = uc' "tau_W" (nounPhraseSP "ODE parameter for water")
  "Derived parameter based on rate of change of temperature of water"
  (sub (Greek Tau_L) cW) second

----------------------
-- Unitless symbols --
----------------------
swhsUnitless :: [ConVar]
swhsUnitless = [uNormalVect, surface, eta, melt_frac]

eta, melt_frac :: ConVar

eta          = cvR (dcc "eta" (nounPhraseSP "ODE parameter")
  "Derived parameter based on rate of change of temperature of water") (Greek Eta_L)

melt_frac    = cvR (dcc "melt_frac" (nounPhraseSP "melt fraction")
  "Ratio of thermal energy to amount of mass melted") --FIXME: Not sure if definition is exactly correct
  (Greek Phi_L)

-----------------
-- Constraints --
-----------------

swhsConstrained ::[ConstrConcept]
swhsConstrained = [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C,
  w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final, 
  temp_PCM, temp_W, w_E, pcm_E]

tank_length, diam, pcm_vol, pcm_SA, pcm_density, temp_melt_P,
  htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C, w_density,
  htCap_W, coil_HTC, pcm_HTC, temp_init, time_final, temp_PCM, 
  temp_W, w_E, pcm_E :: ConstrConcept

-- Input Constraints
  
-- Constraint 1
tank_length  = cuc' "tank_length" (nounPhraseSP "length of tank")
  "The length of the tank" cL metre Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C tank_length_min :<= c :<= C tank_length_max]

-- Constraint 2
diam         = cuc' "diam" (nounPhraseSP "diameter of tank")
  "The diameter of the tank" cD metre Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> (c :/ C tank_length_max) :<=
  (c :/ C tank_length) :<= (c :/ C tank_length_min)]

-- Constraint 3
pcm_vol      = cuc' "pcm_vol" (nounPhraseSP "volume of PCM")
  "The amount of space occupied by a given quantity of phase change material"
  (sub (vol ^. symbol) cP) m_3 Rational
  [physc $ \c -> c :> Int 0,
  physc $ \c -> c :< C tank_vol]

-- Constraint 4
pcm_SA       = cuc' "pcm_SA"
  (compoundPhrase (nounPhrase'' (phrase phsChgMtrl) (phrase phsChgMtrl) CapFirst CapWords)
  (nounPhrase'' (phrase surArea) (phrase surArea) CapFirst CapWords))
  "Area covered by the outermost layer of the phase change material"
  (sub cA cP) m_2 Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C pcm_vol :<= c :<= ((Int 2 :/ C htTransCoeff_min) :* C tank_vol)]

-- Constraint 5
pcm_density  = cuc' "pcm_density" (nounPhraseSP "density of PCM")
  "Mass per unit volume of the phase change material"
  (sub (density ^. symbol) cP) densityU Rational
  [physc $ \c -> c :> Int 0,
  physc $ \c -> C pcm_density_min :< c :< C pcm_density_max]

-- Constraint 6
temp_melt_P  = cuc' "temp_melt_P" (nounPhraseSP "melting point temperature for PCM")
  "Temperature at which the phase change material transitions from a solid to a liquid"
  (sup (sub (temp ^. symbol) (Atomic "melt")) cP) centigrade Rational
  [physc $ \c -> Int 0 :< c :< C temp_C]

-- Constraint 7
htCap_S_P    = cuc' "htCap_S_P" (nounPhraseSP "specific heat capacity of PCM as a solid")
  ("The amount of energy required to raise the temperature of a " ++
  "given unit mass of solid phase change material by a given amount")
  (sup (sub (heat_cap_spec ^. symbol) cP) cS) UT.heat_cap_spec Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C htCap_S_P_min :< c :< C htCap_S_P_max]

-- Constraint 8
htCap_L_P    = cuc' "htCap_L_P" (nounPhraseSP "specific heat capacity of PCM as a liquid")
  ("The amount of energy required to raise the temperature of a " ++
  "given unit mass of liquid phase change material by a given amount")
  (sup (sub (heat_cap_spec ^. symbol) cP) cL) UT.heat_cap_spec Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C htCap_L_P_min :< c :< C htCap_L_P_max]

--Constraint 9
htFusion     = cuc' "htFusion" (nounPhraseSP "specific latent heat of fusion")
  ("amount of thermal energy required to completely melt a unit mass of a substance")
  (sub cH lF) specificE Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C htFusion_min :< c :< C htFusion_max]

-- Constraint 10
coil_SA      = cuc' "coil_SA"
  (compoundPhrase (nounPhrase'' (phrase coil) (phrase coil) CapFirst CapWords)
  (nounPhrase'' (phrase surArea) (phrase surArea) CapFirst CapWords))
  "Area covered by the outermost layer of the coil" (sub cA cC) m_2 Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> c :<= C coil_SA_max]

-- Constraint 11
temp_C       = cuc' "temp_C" (nounPhraseSP "temperature of the heating coil")
  "The average kinetic energy of the particles within the coil"
  (sub (temp ^. symbol) cC) centigrade Rational
  [physc $ \c -> Int 0 :< c :< Int 100]

-- Constraint 12
w_density    = cuc' "w_density" (density `of_` water)
  "Mass per unit volume of water" (sub (density ^. symbol) cW) densityU Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C w_density_min :< c :<= C w_density_max]
  
-- Constraint 13
htCap_W      = cuc' "htCap_W" (heat_cap_spec `of_` water)
  "The amount of energy required to raise the temperature of a given unit mass of water by a given amount"
  (sub (heat_cap_spec ^. symbol) cW) UT.heat_cap_spec Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C htCap_W_min :< c :< C htCap_W_max]
  
-- Constraint 14
coil_HTC     = cuc' "coil_HTC" (nounPhraseSP
  "convective heat transfer coefficient between coil and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the coil to the surrounding water") (sub (htTransCoeff ^. symbol) cC)
  UT.heat_transfer_coef Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C coil_HTC_min :<= c :<= C coil_HTC_max]
  
-- Constraint 15
pcm_HTC      = cuc' "pcm_HTC"
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the phase change material to the surrounding water")
  (sub lH cP) UT.heat_transfer_coef Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> C pcm_HTC_min :<= c :<= C pcm_HTC_max]
  
-- Constraint 16
temp_init    = cuc' "temp_init" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (temp ^. symbol)(Atomic "init")) centigrade Rational
  [physc $ \c -> 0 :< c :< C melt_pt]
  
-- Constraint 17
time_final   = cuc' "time_final" (nounPhraseSP "final time")
  ("The amount of time elapsed from the beginning of the " ++
  "simulation to its conclusion") (sub (time ^. symbol) 
  (Atomic "final")) second Rational
  [physc $ \c -> c :> Int 0,
  sfwrc $ \c -> c :< C time_final_max]
  
  
-- Output Constraints

-- Constraint 18
temp_W       = cuc' "temp_W"
  (nounPhraseSP "temperature of the water")
  "The average kinetic energy of the particles within the water" 
  (sub (temp ^. symbol) cW) centigrade Rational
  [physc $ \c -> C temp_init :<= c :<= C temp_C]

-- Constraint 19
temp_PCM     = cuc' "temp_PCM"
  (nounPhraseSP "temperature of the phase change material" )
  "The average kinetic energy of the particles within the phase change material"
  (sub (temp ^. symbol) cP) centigrade Rational
  [physc $ \c -> C temp_init :<= c :<= C temp_C]
  
-- Constraint 20
w_E          = cuc' "w_E" (nounPhraseSP "change in heat energy in the water")
  "Change in thermal energy within the water" 
  (sub (sens_heat ^. symbol) cW) joule Rational
  [physc $ \c -> c :>= Int 0]
  
-- Constraint 21
pcm_E        = cuc' "pcm_E" (nounPhraseSP "change in heat energy in the PCM")
  "Change in thermal energy within the phase change material" 
  (sub (sens_heat ^. symbol) cP) joule Rational
  [physc $ \c -> c :>= Int 0]



-------------------------
-- Max / Min Variables --
-------------------------

tank_length_min, tank_length_max, htTransCoeff_min, pcm_density_min, 
  pcm_density_max, coil_SA_max, w_density_min, w_density_max, htCap_S_P_min, 
  htCap_S_P_max, htCap_L_P_min, htCap_L_P_max, htFusion_min, htFusion_max,
  htCap_W_min, htCap_W_max, coil_HTC_min, coil_HTC_max, pcm_HTC_min,
  pcm_HTC_max, time_final_max :: UnitaryChunk

-- Used in Constraint 1
tank_length_min = unitary "tank_length_min" (nounPhraseSP "minimum length of tank")
  (sub (tank_length ^. symbol) (Atomic "min")) metre Rational

tank_length_max = unitary "tank_length_max" (nounPhraseSP "maximum length of tank")
  (sub (tank_length ^. symbol) (Atomic "max")) metre Rational

-- Used in Constraint 4
htTransCoeff_min = unitary "htTransCoeff_min"
  (nounPhraseSP "minimum convective heat transfer coefficient")
  (sub (htTransCoeff ^. symbol) (Atomic "min")) UT.heat_transfer_coef Rational

-- Used in Constraint 5
pcm_density_min = unitary "pcm_density_min" (nounPhraseSP "minimum density of PCM")
  (sup (pcm_density ^. symbol) (Atomic "min")) densityU Rational

pcm_density_max = unitary "pcm_density_max" (nounPhraseSP "maximum density of PCM")
  (sup (pcm_density ^. symbol) (Atomic "max")) densityU Rational

-- Used in Constraint 7
htCap_S_P_min = unitary "htCap_S_P_min"
  (nounPhraseSP "minimum specific heat capacity of PCM as a solid")
  (sub (htCap_S_P ^. symbol) (Atomic "min")) UT.heat_cap_spec Rational

htCap_S_P_max = unitary "htCap_S_P_max"
  (nounPhraseSP "maximum specific heat capacity of PCM as a solid")
  (sub (htCap_S_P ^. symbol) (Atomic "max")) UT.heat_cap_spec Rational

-- Used in Constraint 8
htCap_L_P_min = unitary "htCap_L_P_min"
  (nounPhraseSP "minimum specific heat capacity of PCM as a liquid")
  (sub (htCap_L_P ^. symbol) (Atomic "min")) UT.heat_cap_spec Rational

htCap_L_P_max = unitary "htCap_L_P_max"
  (nounPhraseSP "maximum specific heat capacity of PCM as a liquid")
  (sub (htCap_L_P ^. symbol) (Atomic "max")) UT.heat_cap_spec Rational

-- Used in Constraint 9
htFusion_min = unitary "htFusion_min"
  (nounPhraseSP "minimum specific latent heat of fusion")
  (sub (htFusion ^. symbol) (Atomic "min")) UT.heat_cap_spec Rational

htFusion_max = unitary "htFusion_max"
  (nounPhraseSP "maximum specific latent heat of fusion")
  (sub (htFusion ^. symbol) (Atomic "max")) UT.heat_cap_spec Rational

-- Used in Constraint 10
coil_SA_max = unitary "coil_SA_max" (nounPhraseSP "maximum surface area of coil")
  (sup (coil_SA ^. symbol) (Atomic "max")) m_2 Rational

-- Used in Constraint 12
w_density_min = unitary "w_density_min" (nounPhraseSP "minimum density of water")
  (sup (w_density ^. symbol) (Atomic "min")) densityU Rational

w_density_max = unitary "w_density_max" (nounPhraseSP "maximum density of water")
  (sup (w_density ^. symbol) (Atomic "max")) densityU Rational
  
-- Used in Constraint 13
htCap_W_min = unitary "htCap_W_min" (nounPhraseSP "minimum specific heat capacity of water")
  (sup (htCap_W ^. symbol) (Atomic "min")) UT.heat_cap_spec Rational

htCap_W_max = unitary "htCap_W_max" (nounPhraseSP "maximum specific heat capacity of water")
  (sup (htCap_W ^. symbol) (Atomic "max")) UT.heat_cap_spec Rational

-- Used in Constraint 14
coil_HTC_min = unitary "coil_HTC_min" (nounPhraseSP "minimum convective heat transfer coefficient between coil and water")
  (sup (coil_HTC ^. symbol) (Atomic "min")) UT.heat_transfer_coef Rational

coil_HTC_max = unitary "coil_HTC_max" (nounPhraseSP "maximum convective heat transfer coefficient between coil and water")
  (sup (coil_HTC ^. symbol) (Atomic "max")) UT.heat_transfer_coef Rational
  
-- Used in Constraint 15
pcm_HTC_min = unitary "pcm_HTC_min" (nounPhraseSP "minimum convective heat transfer coefficient between PCM and water")
  (sup (pcm_HTC ^. symbol) (Atomic "min")) UT.heat_transfer_coef Rational

pcm_HTC_max = unitary "pcm_HTC_max" (nounPhraseSP "maximum convective heat transfer coefficient between PCM and water")
  (sup (pcm_HTC ^. symbol) (Atomic "max")) UT.heat_transfer_coef Rational
  
-- Used in Constraint 17
time_final_max = unitary "time_final_max" (nounPhraseSP "maximum final time")
  (sup (time_final ^. symbol) (Atomic "max")) second Rational