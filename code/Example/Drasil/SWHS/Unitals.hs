module Drasil.SWHS.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Concepts.Thermodynamics (thermal_energy)
import Data.Drasil.Quantities.Thermodynamics(temp)
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.Math (surface, normalVect)
import Data.Drasil.Quantities.PhysicalProperties (mass, density,)
import Data.Drasil.Units.PhysicalProperties
import Drasil.SWHS.Concepts hiding (fixme)

import Control.Lens ((^.))
import Prelude hiding (id)

swhsSymbols :: [CQSWrapper]
swhsSymbols = (map cqs swhsUnits) ++ (map cqs swhsUnitless)

-- Symbols with Units --

swhsUnits :: [UCWrapper]
swhsUnits = map ucw [coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,
  htCap_S_P,htCap_V,htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,
  htTransCoeff,coil_HTC,pcm_HTC,tank_length,pcm_mass,w_mass,ht_flux,latentE,
  thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,latentE_P,temp,
  temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,w_vol,deltaT,
  density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W] ++ 
  map ucw [htFusion, mass, time]

coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,htCap_S_P,htCap_V,
  htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,htTransCoeff,coil_HTC,
  pcm_HTC,tank_length,pcm_mass,w_mass,ht_flux,latentE,
  thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,latentE_P,
  temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,w_vol,deltaT,
  pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W :: UnitalChunk
  
htFusion :: UnitalChunk  

--symbol names can't begin with a capital
fixme :: String
fixme = "FIXME: Define this or remove the need for definitions"

coil_SA      = uc' "coil_SA" (nounPhraseSP "coil surface area") fixme 
  (sub cA cC) m_2
in_SA        = uc' "in_SA" (nounPhraseSP 
  "surface area over which heat is transferred in")
  fixme (sub cA (Atomic "in")) m_2
out_SA       = uc' "out_SA" (nounPhraseSP 
  "surface area over which heat is transferred out")
  fixme (sub cA (Atomic "out")) m_2
pcm_SA       = uc' "pcm_SA" (nounPhraseSP "phase change material surface area")
  fixme (sub cA cP) m_2
htCap        = uc specific_heat cC heat_cap_spec
htCap_L      = uc' "htCap_L" (nounPhraseSP "specific heat capacity of a liquid")
  fixme (sup (htCap ^. symbol) cL) heat_cap_spec
htCap_L_P    = uc' "htCap_L_P" (nounPhraseSP 
  "specific heat capacity of PCM as a liquid")
  fixme (sup (sub (htCap ^. symbol) cP) cL) heat_cap_spec
htCap_S      = uc' "htCap_S" 
  (nounPhraseSP "specific heat capacity of a solid")
  fixme (sup (htCap ^. symbol) cS) heat_cap_spec
htCap_S_P    = uc' "htCap_S_P" 
  (nounPhraseSP "specific heat capacity of PCM as a solid")
  fixme (sup (sub (htCap ^. symbol) cP) cS) heat_cap_spec
htCap_V      = uc' "htCap_V" 
  (nounPhraseSP "specific heat capacity of a vapour")
  fixme (sup (htCap ^. symbol) cV) heat_cap_spec
htCap_W      = uc' "htCap_W" 
  (nounPhraseSP "specific heat capacity of water")
  fixme (sub (htCap ^. symbol) cW) heat_cap_spec
diam         = uc' "diam" 
  (nounPhraseSP "diameter of tank") fixme cD metre
sensHtE      = uc' "sensHtE" 
  (nounPhraseSP "sensible heat energy") fixme cE joule
pcm_initMltE = uc' "pcm_initMltE" (nounPhraseSP 
  "change in heat energy in the PCM at the instant when melting begins")
  fixme (sup (sub (sensHtE ^. symbol) (Atomic "Pmelt")) (Atomic "init")) joule
pcm_E        = uc' "pcm_E" (nounPhraseSP "change in heat energy in the PCM")
  fixme (sub (sensHtE ^. symbol) cP) joule
w_E          = uc' "w_E" (nounPhraseSP "change in heat energy in the water")
  fixme (sub (sensHtE ^. symbol) cW) joule
vol_ht_gen   = uc' "vol_ht_gen" 
  (nounPhraseSP "volumetric heat generation per unit volume")
  fixme lG volHtGenU 
htTransCoeff = uc' "htTransCoeff" 
  (nounPhraseSP "convective heat transfer coefficient")
  fixme lH heat_transfer_coef
coil_HTC     = uc' "coil_HTC" (nounPhraseSP 
  "convective heat transfer coefficient between coil and water")
  fixme (sub (htTransCoeff ^. symbol) cC) heat_transfer_coef
htFusion     = makeUCWDS "htFusion" (nounPhraseSP 
  "specific latent heat of fusion")
  (S "amount of " :+: (phrase $ thermal_energy ^. term) +:+
  S "required to completely melt a unit " :+: (phrase $ mass ^. term) +:+
  S "of a substance.") (sub cH lF) specificE
pcm_HTC      = uc' "pcm_HTC" 
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  fixme (sub lH cP) heat_transfer_coef
tank_length  = uc' "tank_length" (nounPhraseSP "length of tank") fixme cL metre
pcm_mass     = uc' "pcm_mass" (nounPhraseSP "mass of phase change material")
  fixme (sub (mass ^. symbol) cP) kilogram
w_mass       = uc' "w_mass" (nounPhraseSP "mass of water")
  fixme (sub (mass ^. symbol) cW) kilogram
ht_flux      = uc heat_flux lQ thermal_flux
latentE      = uc' "latentE" (nounPhraseSP "latent heat energy") fixme cQ joule
thFluxVect   = uc' "thFluxVect" (nounPhraseSP "thermal flux vector") 
  fixme (vec lQ) thermal_flux
ht_flux_C    = uc' "ht_flux_C" 
  (nounPhraseSP "heat flux into the water from the coil") 
  fixme (sub (ht_flux ^. symbol) cC) thermal_flux
ht_flux_in   = uc' "ht_flux_in" (nounPhraseSP "heat flux input")
  fixme (sub (ht_flux ^. symbol) (Atomic "in")) thermal_flux
ht_flux_out  = uc' "ht_flux_out" (nounPhraseSP "heat flux output")
  fixme (sub (ht_flux ^. symbol) (Atomic "out")) thermal_flux
ht_flux_P    = uc' "ht_flux_P" (nounPhraseSP "heat flux into the PCM from water") 
  fixme (sub (ht_flux ^. symbol) cP) thermal_flux
latentE_P    = uc' "latentE_P" (nounPhraseSP "latent heat energy added to PCM")
  fixme (sub (latentE ^. symbol) cP) joule
temp_boil    = uc' "temp_boil" (nounPhraseSP "boiling point temperature")
  fixme (sub (temp ^. symbol) (Atomic "boil")) centigrade
temp_C       = uc' "temp_C" (nounPhraseSP "temperature of the heating coil") 
  fixme (sub (temp ^. symbol) cC) centigrade
temp_env     = uc' "temp_env" (nounPhraseSP "temperature of the environment") 
  fixme (sub (temp ^. symbol) (Atomic "env")) centigrade
time_final   = uc' "time_final" (nounPhraseSP "final time")
  fixme (sub (time ^. symbol) (Atomic "final")) second
temp_init    = uc' "temp_init" (nounPhraseSP "initial temperature")
  fixme (sub (temp ^. symbol) (Atomic "init")) centigrade
temp_melt    = uc' "temp_melt" (nounPhraseSP "melting point temperature")
  fixme (sub (temp ^. symbol) (Atomic "melt")) centigrade
t_init_melt  = uc' "t_init_melt" 
  (nounPhraseSP "time at which melting of PCM begins")
  fixme (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "init")) second
t_final_melt = uc' "t_final_melt" 
  (nounPhraseSP "time at which melting of PCM ends")
  fixme (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "final")) second
temp_melt_P  = uc' "temp_melt_P" 
  (nounPhraseSP "melting point temperature for PCM")
  fixme (sup (sub (temp ^. symbol) (Atomic "melt")) cP) centigrade
temp_PCM     = uc' "temp_PCM" 
  (nounPhraseSP "temperature of the phase change material" )
  fixme (sub (temp ^. symbol) cP) centigrade
temp_W       = uc' "temp_W" 
  (nounPhraseSP "temperature of the water")
  fixme (sub (temp ^. symbol) cW) centigrade
volume       = uc' "volume" (cn' "volume") fixme cV m_3
pcm_vol      = uc' "pcm_vol" (nounPhraseSP "volume of PCM")
  fixme (sub (volume ^. symbol) cP) m_3
tank_vol     = uc' "tank_vol" (nounPhraseSP "volume of the cylindrical tank")
  fixme (sub (volume ^. symbol) (Atomic "tank")) m_3
w_vol        = uc' "w_vol" (nounPhraseSP "volume of water")
  fixme (sub (volume ^. symbol) cW) m_3
deltaT       = uc' "deltaT" (nounPhraseSP "change in temperature")
  fixme (Concat [Greek Delta, (temp ^. symbol)]) centigrade
pcm_density  = uc' "pcm_density" (nounPhraseSP "density of PCM")
  fixme (sub (density ^. symbol) cP) densityU
w_density    = uc' "w_density" (nounPhraseSP "density of water")
  fixme (sub (density ^. symbol) cW) densityU
tau          = uc' "tau" (nounPhraseSP "dummy variable for integration over time") 
  fixme (Greek Tau_L) second
tau_L_P      = uc' "tau_L_P" (nounPhraseSP "ODE parameter for liquid PCM")
  fixme (sup (sub (Greek Tau_L) cP) cL) second
tau_S_P      = uc' "tau_S_P" (nounPhraseSP "ODE parameter for solid PCM")
  fixme (sup (sub (Greek Tau_L) cP) cS) second
tau_W        = uc' "tau_W" (nounPhraseSP "ODE parameter for water")
  fixme (sub (Greek Tau_L) cW) second

-- Unitless symbols --

swhsUnitless :: [ConVar]
swhsUnitless = [normalVect, surface, eta, melt_frac]

eta, melt_frac :: ConVar
eta          = cvR (dcc "eta" (nounPhraseSP "ODE parameter") fixme) (Greek Eta_L)
melt_frac    = cvR (dcc "melt_frac" (nounPhraseSP "melt fraction") fixme) 
  (Greek Phi_L)
