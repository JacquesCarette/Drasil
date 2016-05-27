{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.Example where

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

---Acronyms---
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,
  phsChgMtrl,physSyst,requirement,srs,progName,thModel]

assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,phsChgMtrl,
  physSyst,requirement,srs,progName,thModel :: ConceptChunk

assumption  = makeCC "A" "Assumption"
dataDefn    = makeCC "DD" "Data Definition"
genDefn     = makeCC "GD" "General Definition"
goalStmt    = makeCC "GS" "Goal Statement"
inModel     = makeCC "IM" "Instance Model"
likelyChg   = makeCC "LC" "Likely Change"
ordDiffEq   = makeCC "ODE" "Ordinary Differential Equation"
phsChgMtrl  = makeCC "PCM" "Phase Change Material"
physSyst    = makeCC "PS" "Physical System Description"
requirement = makeCC "R" "Requirement"
srs         = makeCC "SRS" "Software Requirements Specification"
progName    = makeCC "SWHS" "Solar Water Heating System"
thModel     = makeCC "T" "Theoretical Model"

---ConceptChunks---

heat_flux, phase_change_material, specific_heat, thermal_conduction,
  transient :: ConceptChunk

heat_flux = makeCC "Heat flux" "The rate of heat energy transfer per unit area."
phase_change_material = makeCC "Phase Change Material (PCM)" "A substance that uses phase changes (melting) to absorb or release large amounts of heat at a constant temperature"
specific_heat = makeCC "Specific heat" "Heat capacity per unit mass."
thermal_conduction = makeCC "Thermal conduction" "The transfer of heat energy through a substance."
transient = makeCC "Transient" "Changing with time."

---EqChunks---
--Theoretical Models--
t1ConsThermE :: RelationChunk
t1ConsThermE = makeRC "Conservation of thermal energy" t1descr consThermERel

consThermERel :: Relation
consThermERel = (Neg (C gradient)) :. (C thFluxVect) + (C vol_ht_gen) :=
                (C density) * (C htCap) * (Deriv (C temp) (C time))

t1descr :: Sentence
t1descr = (S "The above equation gives the conservation of energy for time" :+:
          S " varying heat transfer in a material of " :+: (htCap ^. descr) :+: S " " :+:
          U (htCap ^. symbol) :+: S " and " :+: (density ^. descr) :+: S " " :+: U (density ^. symbol) :+:
          S ", where " :+: U (thFluxVect ^. symbol) :+: S " is the " :+: (thFluxVect ^. descr) :+:
          S ", " :+: U (vol_ht_gen ^. symbol) :+: S " is the " :+: (vol_ht_gen ^. descr) :+:
          S ", " :+: U (temp ^. symbol) :+: S " is the " :+: (temp ^. descr) :+: S ", " :+:
          U (time ^. symbol) :+: S " is " :+: (time ^. descr) :+: S ", and " :+: U (gradient ^. symbol) :+:
          S " is the " :+: (gradient ^. descr) :+: S ". For this equation to apply, " :+:
          S "other forms of energy, such as mechanical energy, are assmed to be negligible" :+:
          S " in the system. (A1)")

--Replace time-varying with transient?
--Should I try to reference everything? Would need to adjust descr of density for it to make sense.
--Looks like referencing within a simple list is not yet implemented.
--Forgot many "S" and ":+:" typing out above description, lost a lot of time fixing

t2SensHtE :: RelationChunk
t2SensHtE = makeRC "Sensible heat energy" t2descr sensHtEEqn

sensHtEEqn :: Relation
sensHtEEqn = (C sensHtE) := (C htCap_S) * (C mass) * (C deltaT)

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

t2descr :: Sentence
t2descr = (U (sensHtE ^. symbol) :+: S " is the change in sensible heat " :+:
          S "energy (" :+: Sy (joule ^. unit) :+: S "). " :+: U (htCap_S ^. symbol) :+: 
          S ", " :+: U (htCap_L ^. symbol) :+: S ", " :+: U (htCap_V ^. symbol) :+:
          S " are the specific heat capacities of a solid, liquid, and vapour, " :+:
          S "respectively. " :+: U (mass ^. symbol) :+: S " is the mass. " :+:
          U (temp ^. symbol) :+: S " is the temperature, and " :+: U (deltaT ^. symbol) :+:
          S " is the change in temperature. " :+: U (temp_melt ^. symbol) :+:
          S " and " :+: U (temp_boil ^. symbol) :+: S " are the melting and" :+:
          S " boiling points, respectively. Sensible heating occurs as long" :+:
          S " as the material does not reach a temperature where a phase " :+:
          S "change occurs. A phase change occurs if " :+: U (temp ^. symbol) :+:
          S "=" :+: U (temp_boil ^. symbol) :+: S " or " :+: U (temp ^. symbol) :+:
          S "=" :+: U (temp_melt ^. symbol) :+: S ". If this is the case, " :+:
          S "refer to T3, Latent heat energy.")

--Can't reference sections from SWHSBody!!
----importing would cause a cyclic import
--How to have new lines in the description? 
--Can't have relation and equation chunks together since they are called in a list
----You can, you just can't map "Definition" over a list, you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?

t3LatHtE :: RelationChunk
t3LatHtE = makeRC "Latent heat energy" t3descr latHtEEqn

latHtEEqn :: Relation
latHtEEqn = (C latentE) := Deriv (C latentE) (C tau)

-- How to have integrals in equations
-- How to have conditions on a single equation

t3descr :: Sentence
t3descr = (U (latentE ^. symbol) :+: S " is the change in thermal energy (" :+:
          Sy (joule ^. unit) :+: S "), latent heat energy. <Integral> is " :+:
          S "the rate of change of " :+: U (latentE ^. symbol) :+: S " with" :+:
          S " respect to time " :+: U (tau ^. symbol) :+: S ". " :+: U (time ^. symbol) :+:
          S " is the time elapsed, as long as the phase change is not " :+:
          S "complete. The status of the phase change depends on the melt" :+:
          S " fraction DD4. Latent heating stops when all material has " :+:
          S "changed to the new phase.")


--General Definitions--
-- gd1NewtonCooling :: RelationChunk
-- gd1NewtonCooling = makeRC "Newton's law of cooling" gd1descr newtonCoolEqn

-- newtonCoolEqn :: Relation
-- newtonCoolEqn = (C ht_flux) * ((C time)) := 
                -- (C htTransCoeff) * (C delta) * (C temp) * ((C time))

-- gd1descr :: Sentence
-- gd1descr = (S "Newton's law of cooling describes convective cooling from a surface...")

--Data Definitions--
dd1HtFluxC :: EqChunk
dd1HtFluxC = fromEqn "Heat flux out of coil" dd1descr (ht_flux_C ^. symbol) thermFluxU htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - (C temp_W))

--Remember to add function calls here when available.

dd1descr :: Sentence
dd1descr = (S "heat flux out of the coil.")

-- dd1descr :: Sentence
-- dd1descr = (U (temp_C ^. symbol) :+: S " is the temperature of the coil. " :+:
           -- U (temp_W ^. symbol) :+: S " is the temperature of the water. " :+:
           -- S "The heat flux out of the coil, " :+: U (ht_flux_C ^. symbol) :+:
           -- S ", is found by assuming that Newton's Law of Cooling applies" :+:
           -- S " (A7). This law (GD1) is used on the surface of the coil, " :+:
           -- S "which has area " :+: U (coil_SA ^. symbol) :+: S " and heat" :+:
           -- S " transfer coefficient " :+: U (coil_HTC ^. symbol) :+: S "." :+:
           -- S " This equation assumes that the temperature of the coil is " :+:
           -- S "constant over time (A8) and that it does not vary along the " :+:
           -- S "length of the coil (A9).")

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: EqChunk
dd2HtFluxP = fromEqn "Heat flux into PCM" dd2descr (ht_flux_P ^. symbol) thermFluxU htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * ((C temp_W) * (C time) - (C temp_PCM) * (C time))

dd2descr :: Sentence
dd2descr = (S "heat flux into the PCM.")

dd3HtFusion :: EqChunk
dd3HtFusion = fromEqn "Specific latent heat of fusion" dd3descr (htFusion ^. symbol) specificE htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latentE) / (C mass)

dd3descr :: Sentence
dd3descr = (S "amount og heat energy required to completely melt a unit " :+:
           S "mass of a substance.")

dd4MeltFrac :: EqChunk
dd4MeltFrac = fromEqn "Melt fraction" dd4descr (melt_frac ^. symbol) unitless melt_frac_eqn

melt_frac_eqn :: Expr
melt_frac_eqn = (C latentE_P) / ((C htFusion) * (C pcm_mass))

dd4descr :: Sentence
dd4descr = (S "fraction of the PCM that is liquid.")