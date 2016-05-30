{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.Unitals where

import Example.Drasil.SWHS.Units

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

swhsSymbols :: [UnitalChunk]
swhsSymbols = [coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,
  htCap_S_P,htCap_V,htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,
  htTransCoeff,coil_HTC,htFusion,pcm_HTC,tank_length,mass,pcm_mass,w_mass,norm_vect,
  ht_flux,latentE,thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,
  latentE_P,time,temp,temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,
  t_init_melt,t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,
  w_vol,deltaT,density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W,melt_frac]

coil_SA,in_SA,out_SA,pcm_SA,htCap,htCap_L,htCap_L_P,htCap_S,
  htCap_S_P,htCap_V,htCap_W,diam,sensHtE,pcm_initMltE,pcm_E,w_E,vol_ht_gen,
  htTransCoeff,coil_HTC,htFusion,pcm_HTC,tank_length,mass,pcm_mass,w_mass,norm_vect,
  ht_flux,latentE,thFluxVect,ht_flux_C,ht_flux_in,ht_flux_out,ht_flux_P,
  latentE_P,time,temp,temp_boil,temp_C,temp_env,time_final,temp_init,temp_melt,
  t_init_melt,t_final_melt,temp_melt_P,temp_PCM,temp_W,volume,pcm_vol,tank_vol,
  w_vol,deltaT,density,pcm_density,w_density,tau,tau_L_P,tau_S_P,tau_W,melt_frac :: UnitalChunk

--symbol names can't begin with a capital

coil_SA      = makeUC "A_C" "coil surface area" (sub cA cC) m_2
in_SA        = makeUC "A_in" "surface area over which heat is transferred in" 
               (sub cA (Atomic "in")) m_2
out_SA       = makeUC "A_out" "surface area over which heat is transferred out"
               (sub cA (Atomic "out")) m_2
pcm_SA       = makeUC "A_P" "phase change material surface area" (sub cA cP) m_2
htCap        = makeUC "C" "specific heat capacity" cC heat_capacity
htCap_L      = makeUC "C^L" "specific heat capacity of a liquid" (sup cC cL) 
               heat_capacity
htCap_L_P    = makeUC "C^L_P" "specific heat capacity of PCM as a liquid"
               (sup (sub cC cP) cL) heat_capacity
htCap_S      = makeUC "C^S" "specific heat capacity of a solid" (sup cC cS)
               heat_capacity
htCap_S_P    = makeUC "C^S_P" "specific heat capacity of PCM as a solid"
              (sup (sub cC cP) cS) heat_capacity
htCap_V      = makeUC "C^V" "specific heat capacity of a vapour" (sup cC cV)
               heat_capacity
htCap_W      = makeUC "C_W" "specific heat capacity of water" (sub cC cW) heat_capacity
diam         = makeUC "D" "diameter of tank" cD metre
sensHtE      = makeUC "E" "sensible heat energy" cE joule
pcm_initMltE = makeUC "E^init_Pmelt" ("heat energy in the PCM at the instant " ++
               "when melting begins") (sup (sub cE (Atomic "Pmelt")) (Atomic "init")) joule
pcm_E        = makeUC "E_P" "heat energy in the PCM" (sub cE cP) joule
w_E          = makeUC "E_W" "heat energy in the water" (sub cE cW) joule
vol_ht_gen   = makeUC "g" "volumetric heat generation per unit volume" lG 
               thermFluxU 
htTransCoeff = makeUC "h" "convective heat transfer coefficient" lH heat_transfer
coil_HTC     = makeUC "h_C" "convctive heat transfer coefficient between coil and water"
               (sub lH cC) heat_transfer
htFusion     = makeUC "H_f" "specific latent heat of fusion" (sub cH lF) specificE
pcm_HTC      = makeUC "h_P" "convective heat transfer coefficient between PCM and water"
               (sub lH cP) heat_transfer
tank_length  = makeUC "L" "length of tank" cL metre
mass         = makeUC "m" "mass" lM kilogram
pcm_mass     = makeUC "m_P" "mass of phase change material" (sub lM cP) kilogram
w_mass       = makeUC "m_W" "mass of water" (sub lM cW) kilogram
norm_vect    = makeUC "n_vect" "unit outward normal vector for a surface"
               (vec $ hat lN) unitless
--In example this was a varChunk for some reason
--I can't do that or else it wouldn't be included in the list
--What to do about units if unitless?
ht_flux      = makeUC "q" "heat flux" lQ thermFluxU
latentE      = makeUC "Q" "latent heat energy" cQ joule
thFluxVect   = makeUC "q_vect" "thermal flux vector" (vec lQ) thermFluxU
ht_flux_C    = makeUC "q_C" "heat flux from coil" (sub lQ cC) thermFluxU
ht_flux_in   = makeUC "q_in" "heat flux in" (sub lQ (Atomic "in")) thermFluxU
ht_flux_out  = makeUC "q_out" "heat flux out" (sub lQ (Atomic "out")) thermFluxU
ht_flux_P    = makeUC "q_P" "heat flux into phase change material" (sub lQ cP) 
               thermFluxU
latentE_P    = makeUC "Q_P" "latent heat energy added to PCM" (sub cQ cP) joule
time         = makeUC "t" "time" lT second 
temp         = makeUC "T" "temperature" cT centigrade
temp_boil    = makeUC "T_boil" "temperature at boiling point" (sub cT (Atomic "boil"))
               centigrade
temp_C       = makeUC "T_C" "temperature of coil" (sub cT cC) centigrade
temp_env     = makeUC "T_env" "temperature of environment" (sub cT (Atomic "env"))
               centigrade
time_final   = makeUC "t_final" "final time" (sub lT (Atomic "final")) second
temp_init    = makeUC "T_init" "initial temperature" (sub cT (Atomic "init"))
               centigrade
temp_melt    = makeUC "T_melt" "temperature at melting point" (sub cT (Atomic "melt"))
               centigrade
t_init_melt  = makeUC "t^init_melt" "time at which melting of PCM begins"
               (sup (sub lT (Atomic "melt")) (Atomic "init")) second
t_final_melt = makeUC "t^final_melt" "time at which melting of PCM ends"
               (sup (sub lT (Atomic "melt")) (Atomic "final")) second
temp_melt_P  = makeUC "T^P_melt" "temperature at melting point for PCM"
               (sup (sub cT (Atomic "melt")) cP) centigrade
temp_PCM     = makeUC "T_P" "temperature of phase change material" (sub cT cP)
               centigrade
temp_W       = makeUC "temp_W" "temperature of water" (sub cT cW) centigrade
volume       = makeUC "V" "volume" cV m_3
pcm_vol      = makeUC "V_P" "volume of PCM" (sub cV cP) m_3
tank_vol     = makeUC "V_tank" "volume of the cylindrical tank" (sub cV (Atomic "tank"))
               m_3
w_vol        = makeUC "V_W" "volume of water" (sub cV cW) m_3
deltaT       = makeUC "deltaT" "temperature difference" (Concat [Special Delta, cT]) 
               centigrade
-- eta          = makeUC "eta" "a constant" (Special Eta_L) unitless
density      = makeUC "rho" "density, mass per unit volume" (Special Rho_L)
               densityU
pcm_density  = makeUC "rho_P" "density of PCM" (sub (Special Rho_L) cP)
               densityU
w_density    = makeUC "rho_W" "density of water" (sub (Special Rho_L) cW)
               densityU
tau          = makeUC "tau" "dummy variable for integration over time" 
               (Special Tau_L) second
tau_L_P      = makeUC "tau_L_P" "a constant" (sup (sub (Special Tau_L) cP) cL) second
tau_S_P      = makeUC "tau_S_P" "a constant" (sup (sub (Special Tau_L) cP) cS) second
tau_W        = makeUC "tau_W" "a constant" (sub (Special Tau_L) cW) second
melt_frac    = makeUC "phi" "melt fraction" (Special Phi_L) unitless

--Created a "unitless" unit in SWHSUnits.hs so that i didn't need varChunks.

--Need to add eta to unicode

--Units are stored in another file. Will these be universal?
--I.e. Anytime someone writes a program involving heat capacity will
--they be able to call "heat_capacity" without having to write the
--code for that unit in a separate file

--VarChunks--
gradient :: VarChunk
gradient = makeVC "gradient" "gradient operator" (Special Nabla)

--General Definitions--
-- gd1NewtonCooling :: RelationChunk
-- gd1NewtonCooling = makeRC "Newton's law of cooling" gd1descr newtonCoolEqn

-- newtonCoolEqn :: Relation
-- newtonCoolEqn = (C ht_flux) * ((C time)) := 
                -- (C htTransCoeff) * (C delta) * (C temp) * ((C time))

-- gd1descr :: Sentence
-- gd1descr = (S "Newton's law of cooling describes convective cooling from a surface...")
