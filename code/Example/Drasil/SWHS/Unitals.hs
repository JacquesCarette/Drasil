{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Drasil.SWHS.Unitals where

import Drasil.SWHS.Units

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

swhsUnits :: [UnitalChunk]
swhsUnits = [coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,
  htCap_S_P,htCap_V,htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,
  htTransCoeff,coil_HTC,htFusion,pcm_HTC,tank_length,mass,
  pcm_mass,w_mass,ht_flux,latentE,thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,
  ht_flux_P,latentE_P,time,temp,temp_boil,temp_C,temp_env,time_final,temp_init,
  temp_melt,t_init_melt,t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,
  tank_vol,w_vol,deltaT,density,pcm_density,w_density,tau,tau_L_P,tau_S_P,
  tau_W]

coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,htCap_S_P,htCap_V,
  htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,htTransCoeff,coil_HTC,
  htFusion,pcm_HTC,tank_length,mass,pcm_mass,w_mass,ht_flux,latentE,
  thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,latentE_P,time,
  temp,temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,t_init_melt,
  t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,w_vol,deltaT,
  density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W :: UnitalChunk

--symbol names can't begin with a capital

coil_SA      = makeUC "coil_SA" "coil surface area" 
  "FIXME: Define this or remove the need for definitions" (sub cA cC) m_2
in_SA        = makeUC "A_in" "surface area over which heat is transferred in" 
  "FIXME: Define this or remove the need for definitions" 
  (sub cA (Atomic "in")) m_2
out_SA       = makeUC "A_out" "surface area over which heat is transferred out"
  "FIXME: Define this or remove the need for definitions"
  (sub cA (Atomic "out")) m_2
pcm_SA       = makeUC "A_P" "phase change material surface area" 
  "FIXME: Define this or remove the need for definitions" (sub cA cP) m_2
htCap        = makeUC "C" "specific heat capacity" 
  "FIXME: Define this or remove the need for definitions" cC heat_cap_spec
htCap_L      = makeUC "C^L" "specific heat capacity of a liquid" 
  "FIXME: Define this or remove the need for definitions" 
  (sup (htCap ^. symbol) cL) heat_cap_spec
htCap_L_P    = makeUC "C^L_P" "specific heat capacity of PCM as a liquid"
  "FIXME: Define this or remove the need for definitions" 
  (sup (sub (htCap ^. symbol) cP) cL) heat_cap_spec
htCap_S      = makeUC "C^S" "specific heat capacity of a solid" 
  "FIXME: Define this or remove the need for definitions" 
  (sup (htCap ^. symbol) cS) heat_cap_spec
htCap_S_P    = makeUC "C^S_P" "specific heat capacity of PCM as a solid"
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (htCap ^. symbol) cP) cS) heat_cap_spec
htCap_V      = makeUC "C^V" "specific heat capacity of a vapour" 
  "FIXME: Define this or remove the need for definitions"
  (sup (htCap ^. symbol) cV) heat_cap_spec
htCap_W      = makeUC "C_W" "specific heat capacity of water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (htCap ^. symbol) cW) heat_cap_spec
diam         = makeUC "D" "diameter of tank" 
  "FIXME: Define this or remove the need for definitions" cD metre
sensHtE      = makeUC "E" "sensible heat energy" 
  "FIXME: Define this or remove the need for definitions" cE joule
pcm_initMltE = makeUC "E^init_Pmelt" 
  "change in heat energy in the PCM at the instant when melting begins"
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (sensHtE ^. symbol) (Atomic "Pmelt")) (Atomic "init")) joule
pcm_E        = makeUC "E_P" "change in heat energy in the PCM" 
  "FIXME: Define this or remove the need for definitions"
  (sub (sensHtE ^. symbol) cP) joule
w_E          = makeUC "E_W" "change in heat energy in the water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (sensHtE ^. symbol) cW) joule
vol_ht_gen   = makeUC "g" "volumetric heat generation per unit volume" 
  "FIXME: Define this or remove the need for definitions" lG volHtGenU 
htTransCoeff = makeUC "h" "convective heat transfer coefficient" 
  "FIXME: Define this or remove the need for definitions" lH heat_transfer_coef
coil_HTC     = makeUC "h_C" 
  "convective heat transfer coefficient between coil and water"
  "FIXME: Define this or remove the need for definitions"
  (sub (htTransCoeff ^. symbol) cC) heat_transfer_coef
htFusion     = makeUCWDS "H_f" "specific latent heat of fusion" 
  (S "amount of " :+: (sLower (thermal_energy ^. term)) :+:
  S " required to completely melt a unit " :+: (mass ^. term) :+:
  S " of a substance.") (sub cH lF) specificE
pcm_HTC      = makeUC "h_P" 
  "convective heat transfer coefficient between PCM and water"
  "FIXME: Define this or remove the need for definitions" 
  (sub lH cP) heat_transfer_coef
tank_length  = makeUC "L" "length of tank" 
  "FIXME: Define this or remove the need for definitions" cL metre
mass         = ucFromVC QPP.mass kilogram
pcm_mass     = makeUC "m_P" "mass of phase change material" 
  "FIXME: Define this or remove the need for definitions"
  (sub (mass ^. symbol) cP) kilogram
w_mass       = makeUC "m_W" "mass of water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (mass ^. symbol) cW) kilogram
ht_flux      = makeUC "q" "heat flux" 
  "FIXME: Define this or remove the need for definitions" lQ thermal_flux
latentE      = makeUC "Q" "latent heat energy" 
  "FIXME: Define this or remove the need for definitions" cQ joule
thFluxVect   = makeUC "q_vect" "thermal flux vector" 
  "FIXME: Define this or remove the need for definitions" (vec lQ) thermal_flux
ht_flux_C    = makeUC "q_C" "heat flux into the water from the coil" 
  "FIXME: Define this or remove the need for definitions"
  (sub (ht_flux ^. symbol) cC) thermal_flux
ht_flux_in   = makeUC "q_in" "heat flux input" 
  "FIXME: Define this or remove the need for definitions"
  (sub (ht_flux ^. symbol) (Atomic "in")) thermal_flux
ht_flux_out  = makeUC "q_out" "heat flux output" 
  "FIXME: Define this or remove the need for definitions"
  (sub (ht_flux ^. symbol) (Atomic "out")) thermal_flux
ht_flux_P    = makeUC "q_P" "heat flux into the PCM from water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (ht_flux ^. symbol) cP) thermal_flux
latentE_P    = makeUC "Q_P" "latent heat energy added to PCM" 
  "FIXME: Define this or remove the need for definitions"
  (sub (latentE ^. symbol) cP) joule
time         = ucFromVC QP.time second
temp         = makeUC "T" "temperature" 
  "FIXME: Define this or remove the need for definitions" cT centigrade
temp_boil    = makeUC "T_boil" "boiling point temperature" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) (Atomic "boil")) centigrade
temp_C       = makeUC "T_C" "temperature of the heating coil" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) cC) centigrade
temp_env     = makeUC "T_env" "temperature of the environment" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) (Atomic "env")) centigrade
time_final   = makeUC "t_final" "final time" 
  "FIXME: Define this or remove the need for definitions"
  (sub (time ^. symbol) (Atomic "final")) second
temp_init    = makeUC "T_init" "initial temperature" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) (Atomic "init")) centigrade
temp_melt    = makeUC "T_melt" "melting point temperature" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) (Atomic "melt")) centigrade
t_init_melt  = makeUC "t^init_melt" "time at which melting of PCM begins"
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "init")) second
t_final_melt = makeUC "t^final_melt" "time at which melting of PCM ends"
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (time ^. symbol) (Atomic "melt")) (Atomic "final")) second
temp_melt_P  = makeUC "T^P_melt" "melting point temperature for PCM"
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (temp ^. symbol) (Atomic "melt")) cP) centigrade
temp_PCM     = makeUC "T_P" "temperature of the phase change material" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) cP) centigrade
temp_W       = makeUC "temp_W" "temperature of the water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (temp ^. symbol) cW) centigrade
volume       = makeUC "V" "volume" 
  "FIXME: Define this or remove the need for definitions" cV m_3
pcm_vol      = makeUC "V_P" "volume of PCM" 
  "FIXME: Define this or remove the need for definitions" 
  (sub (volume ^. symbol) cP) m_3
tank_vol     = makeUC "V_tank" "volume of the cylindrical tank"
  "FIXME: Define this or remove the need for definitions"
  (sub (volume ^. symbol) (Atomic "tank")) m_3
w_vol        = makeUC "V_W" "volume of water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (volume ^. symbol) cW) m_3
deltaT       = makeUC "deltaT" "change in temperature" 
  "FIXME: Define this or remove the need for definitions"
  (Concat [Greek Delta, (temp ^. symbol)]) centigrade
density      = makeUC "density" "density" 
  "FIXME: Define this or remove the need for definitions" (Greek Rho_L) densityU
pcm_density  = makeUC "rho_P" "density of PCM"
  "FIXME: Define this or remove the need for definitions"
  (sub (density ^. symbol) cP) densityU
w_density    = makeUC "rho_W" "density of water"
  "FIXME: Define this or remove the need for definitions"
  (sub (density ^. symbol) cW) densityU
tau          = makeUC "tau" "dummy variable for integration over time" 
  "FIXME: Define this or remove the need for definitions"
  (Greek Tau_L) second
tau_L_P      = makeUC "tau_L_P" "ODE parameter for liquid PCM"
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (Greek Tau_L) cP) cL) second
tau_S_P      = makeUC "tau_S_P" "ODE parameter for solid PCM" 
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (Greek Tau_L) cP) cS) second
tau_W        = makeUC "tau_W" "ODE parameter for water" 
  "FIXME: Define this or remove the need for definitions"
  (sub (Greek Tau_L) cW) second

-- Unitless symbols --

swhsUnitless :: [ConVar]
-- norm_vect used to go here, but due to type change it is no longer included
-- in this list.
swhsUnitless = [norm_vect, QP.surface, eta, melt_frac]

eta, melt_frac :: ConVar

eta          = cvR (dcc "eta" "ODE parameter" 
  "FIXME: Define this or remove the need for definitions") (Greek Eta_L)
melt_frac    = cvR (dcc "melt_frac" "melt fraction"
  "FIXME: Define this or remove the need for definitions") (Greek Phi_L)

--Units are stored in another file. Will these be universal?
--I.e. Anytime someone writes a program involving heat capacity will
--they be able to call "heat_cap_spec" without having to write the
--code for that unit in a separate file?

--General Definitions--
-- gd1NewtonCooling :: RelationChunk
-- gd1NewtonCooling = makeRC "Newton's law of cooling" gd1descr newtonCoolEqn

-- newtonCoolEqn :: Relation
-- newtonCoolEqn = (C ht_flux) * ((C time)) := 
                -- (C htTransCoeff) * (C delta) * (C temp) * ((C time))

-- gd1descr :: Sentence
-- gd1descr = (S "Newton's law of cooling describes convective cooling...")
