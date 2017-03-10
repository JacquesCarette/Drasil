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
  density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W :: UnitalChunk
  
mass, htFusion, time :: UnitalChunk  

--symbol names can't begin with a capital
fixme :: String
fixme = "FIXME: Define this or remove the need for definitions"

coil_SA      = uc' "coil_SA" "coil surface area" fixme (sub cA cC) m_2
in_SA        = uc' "in_SA" "surface area over which heat is transferred in" 
  fixme (sub cA (Atomic "in")) m_2
out_SA       = uc' "out_SA" "surface area over which heat is transferred out"
  fixme (sub cA (Atomic "out")) m_2
pcm_SA       = uc' "pcm_SA" "phase change material surface area" 
  fixme (sub cA cP) m_2
htCap        = uc' "htCap" "specific heat capacity" 
  fixme cC heat_cap_spec
htCap_L      = uc' "htCap_L" "specific heat capacity of a liquid" 
  fixme (sup (htCap ^. symbol) cL) heat_cap_spec
htCap_L_P    = uc' "htCap_L_P" "specific heat capacity of PCM as a liquid"
  fixme (sup (sub (htCap ^. symbol) cP) cL) heat_cap_spec
htCap_S      = uc' "htCap_S" "specific heat capacity of a solid" 
  fixme (sup (htCap ^. symbol) cS) heat_cap_spec
htCap_S_P    = uc' "htCap_S_P" "specific heat capacity of PCM as a solid"
  fixme (sup (sub (htCap ^. symbol) cP) cS) heat_cap_spec
htCap_V      = uc' "htCap_V" "specific heat capacity of a vapour" 
  fixme (sup (htCap ^. symbol) cV) heat_cap_spec
htCap_W      = uc' "htCap_W" "specific heat capacity of water" 
  fixme (sub (htCap ^. symbol) cW) heat_cap_spec
diam         = uc' "diam" "diameter of tank" fixme cD metre
sensHtE      = uc' "sensHtE" "sensible heat energy" fixme cE joule
pcm_initMltE = uc' "pcm_initMltE" 
  "change in heat energy in the PCM at the instant when melting begins"
  fixme (sup (sub (sensHtE ^. symbol) (Atomic "Pmelt")) (Atomic "init")) joule
pcm_E        = uc' "pcm_E" "change in heat energy in the PCM" 
  fixme (sub (sensHtE ^. symbol) cP) joule
w_E          = uc' "w_E" "change in heat energy in the water" 
  fixme (sub (sensHtE ^. symbol) cW) joule
vol_ht_gen   = uc' "vol_ht_gen" "volumetric heat generation per unit volume" 
  fixme lG volHtGenU 
htTransCoeff = uc' "htTransCoeff" "convective heat transfer coefficient" 
  fixme lH heat_transfer_coef
coil_HTC     = uc' "coil_HTC" 
  "convective heat transfer coefficient between coil and water"
  fixme (sub (htTransCoeff ^. symbol) cC) heat_transfer_coef
htFusion     = makeUCWDS "htFusion" "specific latent heat of fusion" 
  (S "amount of " :+: (sLower (thermal_energy ^. term)) :+:
  S " required to completely melt a unit " :+: (mass ^. term) :+:
  S " of a substance.") (sub cH lF) specificE
pcm_HTC      = uc' "pcm_HTC" 
  "convective heat transfer coefficient between PCM and water"
  fixme (sub lH cP) heat_transfer_coef
tank_length  = uc' "tank_length" "length of tank" fixme cL metre
mass         = ucFromVC QPP.mass kilogram
pcm_mass     = uc' "pcm_mass" "mass of phase change material" 
  fixme (sub (mass ^. symbol) cP) kilogram
w_mass       = uc' "w_mass" "mass of water" 
  fixme (sub (mass ^. symbol) cW) kilogram
ht_flux      = uc' "ht_flux" "heat flux" fixme lQ thermal_flux
latentE      = uc' "latentE" "latent heat energy" fixme cQ joule
thFluxVect   = uc' "thFluxVect" "thermal flux vector" 
  fixme (vec lQ) thermal_flux
ht_flux_C    = uc' "ht_flux_C" "heat flux into the water from the coil" 
  fixme (sub (ht_flux ^. symbol) cC) thermal_flux
ht_flux_in   = uc' "ht_flux_in" "heat flux input" 
  fixme (sub (ht_flux ^. symbol) (Atomic "in")) thermal_flux
ht_flux_out  = uc' "ht_flux_out" "heat flux output" 
  fixme (sub (ht_flux ^. symbol) (Atomic "out")) thermal_flux
ht_flux_P    = uc' "ht_flux_P" "heat flux into the PCM from water" 
  fixme (sub (ht_flux ^. symbol) cP) thermal_flux
latentE_P    = uc' "latentE_P" "latent heat energy added to PCM" 
  fixme (sub (latentE ^. symbol) cP) joule
time         = ucFromVC QP.time second
temp         = uc' "temp" "temperature" fixme cT centigrade
temp_boil    = uc' "temp_boil" "boiling point temperature" 
  fixme (sub (temp ^. symbol) (Atomic "boil")) centigrade
temp_C       = uc' "temp_C" "temperature of the heating coil" 
  fixme (sub (temp ^. symbol) cC) centigrade
temp_env     = uc' "temp_env" "temperature of the environment" 
  fixme (sub (temp ^. symbol) (Atomic "env")) centigrade
time_final   = uc' "time_final" "final time" 
  fixme (sub (time ^. symbol) (Atomic "final")) second
temp_init    = uc' "temp_init" "initial temperature" 
  fixme (sub (temp ^. symbol) (Atomic "init")) centigrade
temp_melt    = uc' "temp_melt" "melting point temperature" 
  fixme (sub (temp ^. symbol) (Atomic "melt")) centigrade
t_init_melt  = uc' "t_init_melt" "time at which melting of PCM begins"
  fixme (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "init")) second
t_final_melt = uc' "t_final_melt" "time at which melting of PCM ends"
  fixme (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "final")) second
temp_melt_P  = uc' "temp_melt_P" "melting point temperature for PCM"
  fixme (sup (sub (temp ^. symbol) (Atomic "melt")) cP) centigrade
temp_PCM     = uc' "temp_PCM" "temperature of the phase change material" 
  fixme (sub (temp ^. symbol) cP) centigrade
temp_W       = uc' "temp_W" "temperature of the water" 
  fixme (sub (temp ^. symbol) cW) centigrade
volume       = uc' "volume" "volume" fixme cV m_3
pcm_vol      = uc' "pcm_vol" "volume of PCM" 
  fixme (sub (volume ^. symbol) cP) m_3
tank_vol     = uc' "tank_vol" "volume of the cylindrical tank"
  fixme (sub (volume ^. symbol) (Atomic "tank")) m_3
w_vol        = uc' "w_vol" "volume of water" 
  fixme (sub (volume ^. symbol) cW) m_3
deltaT       = uc' "deltaT" "change in temperature" 
  fixme (Concat [Greek Delta, (temp ^. symbol)]) centigrade
density      = uc' "density" "density" fixme (Greek Rho_L) densityU
pcm_density  = uc' "pcm_density" "density of PCM"
  fixme (sub (density ^. symbol) cP) densityU
w_density    = uc' "w_density" "density of water"
  fixme (sub (density ^. symbol) cW) densityU
tau          = uc' "tau" "dummy variable for integration over time" 
  fixme (Greek Tau_L) second
tau_L_P      = uc' "tau_L_P" "ODE parameter for liquid PCM"
  fixme (sup (sub (Greek Tau_L) cP) cL) second
tau_S_P      = uc' "tau_S_P" "ODE parameter for solid PCM" 
  fixme (sup (sub (Greek Tau_L) cP) cS) second
tau_W        = uc' "tau_W" "ODE parameter for water" 
  fixme (sub (Greek Tau_L) cW) second

-- Unitless symbols --

swhsUnitless :: [ConVar]
swhsUnitless = [norm_vect, QP.surface, eta, melt_frac]

eta, melt_frac :: ConVar
eta          = cvR (dcc "eta" "ODE parameter" fixme) (Greek Eta_L)
melt_frac    = cvR (dcc "melt_frac" "melt fraction" fixme) (Greek Phi_L)
