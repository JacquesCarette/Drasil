module Drasil.SWHS.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Concepts.Thermodynamics (thermal_energy)
import qualified Data.Drasil.Quantities.Physics as QP (surface, time)
import Data.Drasil.Quantities.Math
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)

import Control.Lens ((^.))
import Prelude hiding (id)

swhsSymbols :: [CQSWrapper]
swhsSymbols = (map cqs swhsUnits) ++ (map cqs swhsUnitless)

-- Symbols with Units --

swhsUnits :: [UWrapper]
swhsUnits = map uw [coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,
  htCap_S_P,htCap_V,htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,
  htTransCoeff,coil_HTC] ++ [uw htFusion] ++ map uw [pcm_HTC,tank_length] ++
  [uw mass] ++ map uw [pcm_mass,w_mass,ht_flux,latentE,thFluxVect,ht_flux_C,
  ht_flux_in,ht_flux_out, ht_flux_P,latentE_P] ++ [uw time] ++ map uw [temp,
  temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,w_vol,deltaT,
  density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W]

coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,htCap_S_P,htCap_V,
  htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,htTransCoeff,coil_HTC,
  pcm_HTC,tank_length,pcm_mass,w_mass,ht_flux,latentE,
  thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,latentE_P,
  temp,temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,w_vol,deltaT,
  density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W :: NUChunk
  
mass, htFusion, time :: UnitalChunk  

--symbol names can't begin with a capital

coil_SA      = nu' "coil_SA" "coil surface area" (sub cA cC) m_2
in_SA        = nu' "in_SA" "surface area over which heat is transferred in" 
  (sub cA (Atomic "in")) m_2
out_SA       = nu' "out_SA" "surface area over which heat is transferred out"
  (sub cA (Atomic "out")) m_2
pcm_SA       = nu' "pcm_SA" "phase change material surface area" 
  (sub cA cP) m_2
htCap        = nu' "htCap" "specific heat capacity" 
  cC heat_cap_spec
htCap_L      = nu' "htCap_L" "specific heat capacity of a liquid" 
  (sup (htCap ^. symbol) cL) heat_cap_spec
htCap_L_P    = nu' "htCap_L_P" "specific heat capacity of PCM as a liquid"
  (sup (sub (htCap ^. symbol) cP) cL) heat_cap_spec
htCap_S      = nu' "htCap_S" "specific heat capacity of a solid" 
  (sup (htCap ^. symbol) cS) heat_cap_spec
htCap_S_P    = nu' "htCap_S_P" "specific heat capacity of PCM as a solid"
  (sup (sub (htCap ^. symbol) cP) cS) heat_cap_spec
htCap_V      = nu' "htCap_V" "specific heat capacity of a vapour" 
  (sup (htCap ^. symbol) cV) heat_cap_spec
htCap_W      = nu' "htCap_W" "specific heat capacity of water" 
  (sub (htCap ^. symbol) cW) heat_cap_spec
diam         = nu' "diam" "diameter of tank" cD metre
sensHtE      = nu' "sensHtE" "sensible heat energy" cE joule
pcm_initMltE = nu' "pcm_initMltE" 
  "change in heat energy in the PCM at the instant when melting begins"
  (sup (sub (sensHtE ^. symbol) (Atomic "Pmelt")) (Atomic "init")) joule
pcm_E        = nu' "pcm_E" "change in heat energy in the PCM" 
  (sub (sensHtE ^. symbol) cP) joule
w_E          = nu' "w_E" "change in heat energy in the water" 
  (sub (sensHtE ^. symbol) cW) joule
vol_ht_gen   = nu' "vol_ht_gen" "volumetric heat generation per unit volume" 
  lG volHtGenU 
htTransCoeff = nu' "htTransCoeff" "convective heat transfer coefficient" 
  lH heat_transfer_coef
coil_HTC     = nu' "coil_HTC" 
  "convective heat transfer coefficient between coil and water"
  (sub (htTransCoeff ^. symbol) cC) heat_transfer_coef
htFusion     = makeUCWDS "htFusion" "specific latent heat of fusion" 
  (S "amount of " :+: (sLower (thermal_energy ^. term)) :+:
  S " required to completely melt a unit " :+: (mass ^. term) :+:
  S " of a substance.") (sub cH lF) specificE
pcm_HTC      = nu' "pcm_HTC" 
  "convective heat transfer coefficient between PCM and water"
  (sub lH cP) heat_transfer_coef
tank_length  = nu' "tank_length" "length of tank" cL metre
mass         = ucFromVC QPP.mass kilogram
pcm_mass     = nu' "pcm_mass" "mass of phase change material" 
  (sub (mass ^. symbol) cP) kilogram
w_mass       = nu' "w_mass" "mass of water" 
  (sub (mass ^. symbol) cW) kilogram
ht_flux      = nu' "ht_flux" "heat flux" lQ thermal_flux
latentE      = nu' "latentE" "latent heat energy" cQ joule
thFluxVect   = nu' "thFluxVect" "thermal flux vector" 
  (vec lQ) thermal_flux
ht_flux_C    = nu' "ht_flux_C" "heat flux into the water from the coil" 
  (sub (ht_flux ^. symbol) cC) thermal_flux
ht_flux_in   = nu' "ht_flux_in" "heat flux input" 
  (sub (ht_flux ^. symbol) (Atomic "in")) thermal_flux
ht_flux_out  = nu' "ht_flux_out" "heat flux output" 
  (sub (ht_flux ^. symbol) (Atomic "out")) thermal_flux
ht_flux_P    = nu' "ht_flux_P" "heat flux into the PCM from water" 
  (sub (ht_flux ^. symbol) cP) thermal_flux
latentE_P    = nu' "latentE_P" "latent heat energy added to PCM" 
  (sub (latentE ^. symbol) cP) joule
time         = ucFromVC QP.time second
temp         = nu' "temp" "temperature" cT centigrade
temp_boil    = nu' "temp_boil" "boiling point temperature" 
  (sub (temp ^. symbol) (Atomic "boil")) centigrade
temp_C       = nu' "temp_C" "temperature of the heating coil" 
  (sub (temp ^. symbol) cC) centigrade
temp_env     = nu' "temp_env" "temperature of the environment" 
  (sub (temp ^. symbol) (Atomic "env")) centigrade
time_final   = nu' "time_final" "final time" 
  (sub (time ^. symbol) (Atomic "final")) second
temp_init    = nu' "temp_init" "initial temperature" 
  (sub (temp ^. symbol) (Atomic "init")) centigrade
temp_melt    = nu' "temp_melt" "melting point temperature" 
  (sub (temp ^. symbol) (Atomic "melt")) centigrade
t_init_melt  = nu' "t_init_melt" "time at which melting of PCM begins"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "init")) second
t_final_melt = nu' "t_final_melt" "time at which melting of PCM ends"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "final")) second
temp_melt_P  = nu' "temp_melt_P" "melting point temperature for PCM"
  (sup (sub (temp ^. symbol) (Atomic "melt")) cP) centigrade
temp_PCM     = nu' "temp_PCM" "temperature of the phase change material" 
  (sub (temp ^. symbol) cP) centigrade
temp_W       = nu' "temp_W" "temperature of the water" 
  (sub (temp ^. symbol) cW) centigrade
volume       = nu' "volume" "volume" cV m_3
pcm_vol      = nu' "pcm_vol" "volume of PCM" 
  (sub (volume ^. symbol) cP) m_3
tank_vol     = nu' "tank_vol" "volume of the cylindrical tank"
  (sub (volume ^. symbol) (Atomic "tank")) m_3
w_vol        = nu' "w_vol" "volume of water" 
  (sub (volume ^. symbol) cW) m_3
deltaT       = nu' "deltaT" "change in temperature" 
  (Concat [Greek Delta, (temp ^. symbol)]) centigrade
density      = nu' "density" "density" (Greek Rho_L) densityU
pcm_density  = nu' "pcm_density" "density of PCM"
  (sub (density ^. symbol) cP) densityU
w_density    = nu' "w_density" "density of water"
  (sub (density ^. symbol) cW) densityU
tau          = nu' "tau" "dummy variable for integration over time" 
  (Greek Tau_L) second
tau_L_P      = nu' "tau_L_P" "ODE parameter for liquid PCM"
  (sup (sub (Greek Tau_L) cP) cL) second
tau_S_P      = nu' "tau_S_P" "ODE parameter for solid PCM" 
  (sup (sub (Greek Tau_L) cP) cS) second
tau_W        = nu' "tau_W" "ODE parameter for water" 
  (sub (Greek Tau_L) cW) second

-- Unitless symbols --

swhsUnitless :: [ConVar]
swhsUnitless = [norm_vect, QP.surface, eta, melt_frac]

eta, melt_frac :: ConVar
eta          = cvR (dcc "eta" "ODE parameter" 
  "FIXME: Define this or remove the need for definitions") (Greek Eta_L)
melt_frac    = cvR (dcc "melt_frac" "melt fraction" 
  "FIXME: Define this or remove the need for definitions") (Greek Phi_L)
