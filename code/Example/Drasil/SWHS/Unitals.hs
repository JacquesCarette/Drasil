module Drasil.SWHS.Unitals where

import Drasil.SWHS.Concepts

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as UT
import Data.Drasil.Concepts.Thermodynamics (thermal_energy)
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.Math (surface, uNormalVect, surArea, diameter)
import Data.Drasil.Quantities.PhysicalProperties (mass, density, vol, len)
import Data.Drasil.Units.PhysicalProperties

import Control.Lens ((^.))
import Prelude hiding (id)

swhsSymbols :: [CQSWrapper]
swhsSymbols = (map cqs swhsUnits) ++ (map cqs swhsUnitless)

-- Symbols with Units --

swhsUnits :: [UCWrapper]
swhsUnits = map ucw [coil_SA,in_SA,out_SA,pcm_SA,heat_cap_spec,htCap_L,htCap_L_P,htCap_S,
  htCap_S_P,htCap_V,htCap_W,diam,sens_heat,pcm_initMltE,pcm_E,w_E,vol_ht_gen,
  htTransCoeff,coil_HTC,pcm_HTC,tank_length,pcm_mass,w_mass,ht_flux,latent_heat,
  thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,latentE_P,temp,
  boil_pt,temp_C,temp_env,time_final,temp_init,melt_pt,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,vol,pcm_vol,tank_vol,w_vol,deltaT,
  density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W] ++ 
  map ucw [htFusion, mass, time]

coil_SA,in_SA,out_SA,pcm_SA,htCap_L,htCap_L_P,htCap_S,htCap_S_P,htCap_V,
  htCap_W,htFusion,diam,pcm_initMltE,pcm_E,w_E,vol_ht_gen,htTransCoeff,coil_HTC,
  pcm_HTC,tank_length,pcm_mass,w_mass,
  thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,latentE_P,
  temp_C,temp_env,time_final,temp_init,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,pcm_vol,tank_vol,w_vol,deltaT,
  pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W :: UnitalChunk

--symbol names can't begin with a capital

coil_SA      = uc' "coil_SA" (compoundPhrase (nounPhrase'' (phrase $ coil ^. term) (phrase $ coil ^. term) CapFirst CapWords) (nounPhrase'' (phrase $ surArea ^. term) (phrase $ surArea ^. term) CapFirst CapWords)) 
  "Area covered by the outermost layer of the coil"(sub cA cC) m_2

in_SA        = uc' "in_SA" (nounPhraseSP 
  "surface area over which heat is transferred in")
  "Surface area over which thermal energy is transferred into an object"
  (sub cA (Atomic "in")) m_2

out_SA       = uc' "out_SA" (nounPhraseSP 
  "surface area over which heat is transferred out")
  "Surface area over which thermal energy is transferred out of an object"
  (sub cA (Atomic "out")) m_2

pcm_SA       = uc' "pcm_SA" (compoundPhrase (nounPhrase'' (phrase $ phsChgMtrl ^. term) (phrase $ phsChgMtrl ^. term) CapFirst CapWords) (nounPhrase'' (phrase $ surArea ^. term) (phrase $ surArea ^. term) CapFirst CapWords))
  "Area covered by the outermost layer of the phase change material" (sub cA cP) m_2

htCap_L      = uc' "htCap_L" (nounPhraseSP "specific heat capacity of a liquid")
  ("The amount of energy required to raise the temperature of a given unit mass of " ++
  "a given liquid by a given amount") (sup (heat_cap_spec ^. symbol) cL) UT.heat_cap_spec

htCap_L_P    = uc' "htCap_L_P" (nounPhraseSP 
  "specific heat capacity of PCM as a liquid")
  ("The amount of energy required to raise the temperature of a given unit mass of liquid " ++
  "phase change material by a given amount") (sup (sub (heat_cap_spec ^. symbol) cP) cL) UT.heat_cap_spec

htCap_S      = uc' "htCap_S" 
  (nounPhraseSP "specific heat capacity of a solid")
  "The amount of energy required to raise the temperature of a given unit mass of a given solid by a given amount"
  (sup (heat_cap_spec ^. symbol) cS) UT.heat_cap_spec

htCap_S_P    = uc' "htCap_S_P" 
  (nounPhraseSP "specific heat capacity of PCM as a solid")
  ("The amount of energy required to raise the temperature of a given unit mass of solid " ++
  "phase change material by a given amount") (sup (sub (heat_cap_spec ^. symbol) cP) cS) UT.heat_cap_spec

htCap_V      = uc' "htCap_V"
  (nounPhraseSP "specific heat capacity of a vapour")
  "The amount of energy required to raise the temperature of a given unit mass of vapour by a given amount"
  (sup (heat_cap_spec ^. symbol) cV) UT.heat_cap_spec

htCap_W      = uc' "htCap_W" 
  (heat_cap_spec `of_` water)
  "The amount of energy required to raise the temperature of a given unit mass of water by a given amount"
  (sub (heat_cap_spec ^. symbol) cW) UT.heat_cap_spec

diam         = uc' "diam" 
  (diameter `of_` tank) "The diameter of the tank" cD metre

pcm_initMltE = uc' "pcm_initMltE" (nounPhraseSP 
  "change in heat energy in the PCM at the instant when melting begins")
  "Change in thermal energy in the phase change material at the melting point"
  (sup (sub (sens_heat ^. symbol) (Atomic "Pmelt")) (Atomic "init")) joule

pcm_E        = uc' "pcm_E" (nounPhraseSP "change in heat energy in the PCM")
  "Change in thermal energy within the phase change material" (sub (sens_heat ^. symbol) cP) joule

w_E          = uc' "w_E" (nounPhraseSP "change in heat energy in the water")
  "Change in thermal energy within the water" (sub (sens_heat ^. symbol) cW) joule

vol_ht_gen   = uc' "vol_ht_gen" 
  (nounPhraseSP "volumetric heat generation per unit volume")
  "Amount of thermal energy generated per unit volume" lG UT.volHtGenU 

htTransCoeff = uc' "htTransCoeff" 
  (nounPhraseSP "convective heat transfer coefficient")
  "The proportionality constant between the heat flux and the thermodynamic driving force for the flow of thermal energy"
  lH UT.heat_transfer_coef

coil_HTC     = uc' "coil_HTC" (nounPhraseSP 
  "convective heat transfer coefficient between coil and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the coil to the surrounding water") (sub (htTransCoeff ^. symbol) cC)
  UT.heat_transfer_coef

htFusion     = makeUCWDS "htFusion" (nounPhraseSP 
  "specific latent heat of fusion")
  (S "amount of " :+: (phrase $ thermal_energy ^. term) +:+
  S "required to completely melt a unit " :+: (phrase $ mass ^. term) +:+
  S "of a substance.") (sub cH lF) specificE

pcm_HTC      = uc' "pcm_HTC" 
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  ("The convective heat transfer coefficient that models " ++
  "the thermal flux from the phase change material to the surrounding water")
  (sub lH cP) UT.heat_transfer_coef

tank_length  = uc' "tank_length" (len `of_` tank)
  "The length of the tank" cL metre

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

temp_C       = uc' "temp_C" (nounPhraseSP "temperature of the heating coil") 
  "The average kinetic energy of the particles within the coil" (sub (temp ^. symbol) cC) centigrade

temp_env     = uc' "temp_env" (nounPhraseSP "temperature of the environment") 
  "The tempature of a given environment" (sub (temp ^. symbol) (Atomic "env")) centigrade

time_final   = uc' "time_final" (nounPhraseSP "final time")
  ("The amount of time elapsed from the beginning of the" ++
  " simulation to its conclusion") (sub (time ^. symbol) (Atomic "final")) second

temp_init    = uc' "temp_init" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation" 
  (sub (temp ^. symbol)(Atomic "init")) centigrade

t_init_melt  = uc' "t_init_melt" 
  (nounPhraseSP "time at which melting of PCM begins")
  "Time at which the phase change material begins changing from a solid to a liquid"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "init")) second

t_final_melt = uc' "t_final_melt" 
  (nounPhraseSP "time at which melting of PCM ends")
  "Time at which the phase change material finishes changes from a solid to a liquid"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "final")) second

temp_melt_P  = uc' "temp_melt_P" 
  (nounPhraseSP "melting point temperature for PCM")
  "Temperature at which the phase change material transitions from a solid to a liquid"
  (sup (sub (temp ^. symbol) (Atomic "melt")) cP) centigrade

temp_PCM     = uc' "temp_PCM" 
  (nounPhraseSP "temperature of the phase change material" )
  "The average kinetic energy of the particles within the phase change material"
  (sub (temp ^. symbol) cP) centigrade

temp_W       = uc' "temp_W" 
  (nounPhraseSP "temperature of the water")
  "The average kinetic energy of the particles within the water" (sub (temp ^. symbol) cW) centigrade

pcm_vol      = uc' "pcm_vol" (nounPhraseSP "volume of PCM")
  "The amount of space occupied by a given quantity of phase change material" (sub (vol ^. symbol) cP) m_3

tank_vol     = uc' "tank_vol" (nounPhraseSP "volume of the cylindrical tank")
  "The amount of space encompassed by a tank" (sub (vol ^. symbol) (Atomic "tank")) m_3

w_vol        = uc' "w_vol" (vol `of_` water)
  "The amount of space occupied by a given quantity of water" (sub (vol ^. symbol) cW) m_3

deltaT       = uc' "deltaT" (nounPhraseSP "change in temperature")
  "Change in the average kinetic energy of a given material"
  (Concat [Greek Delta, (temp ^. symbol)]) centigrade

pcm_density  = uc' "pcm_density" (nounPhraseSP "density of PCM")
  "Mass per unit volume of the phase change material"
  (sub (density ^. symbol) cP) densityU

w_density    = uc' "w_density" (density `of_` water)
  "Mass per unit volume of water" (sub (density ^. symbol) cW) densityU

tau          = uc' "tau" (nounPhraseSP "dummy variable for integration over time") 
  "Binary value representing the presence or absence of integration over time" (Greek Tau_L) second
--Not sure how to define anything after this point

tau_L_P      = uc' "tau_L_P" (nounPhraseSP "ODE parameter for liquid PCM")
  ("Derived through melting of phase change material, which changes ODE parameter"
  ++ "for solid PCM into parameter for liquid") (sup (sub (Greek Tau_L) cP) cL) second

tau_S_P      = uc' "tau_S_P" (nounPhraseSP "ODE parameter for solid PCM")
  "Derived parameter based on rate of change of temperature of phase change material"
  (sup (sub (Greek Tau_L) cP) cS) second

tau_W        = uc' "tau_W" (nounPhraseSP "ODE parameter for water")
  "Derived parameter based on rate of change of temperature of water"
  (sub (Greek Tau_L) cW) second

-- Unitless symbols --

swhsUnitless :: [ConVar]
swhsUnitless = [uNormalVect, surface, eta, melt_frac]

eta, melt_frac :: ConVar

eta          = cvR (dcc "eta" (nounPhraseSP "ODE parameter")
  "Derived parameter based on rate of change of temperature of water") (Greek Eta_L)

melt_frac    = cvR (dcc "melt_frac" (nounPhraseSP "melt fraction")
  "Ratio of thermal energy to amount of mass melted") --FIXME: Not sure if definition is exactly correct
  (Greek Phi_L)
