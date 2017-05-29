module Drasil.NoPCM.Example where

import Language.Drasil

import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as U
import Data.Drasil.Quantities.PhysicalProperties
import Prelude hiding (id)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Concepts.Math (ode, surArea)
import Data.Drasil.Quantities.Math (gradient, diameter)
import Data.Drasil.Units.PhysicalProperties

import Control.Lens ((^.))

pcmSymbols :: [CQSWrapper]
pcmSymbols = map cqs pcmUnits

pcmUnits :: [UCWrapper]
pcmUnits = map ucw [coil_SA, density, dummyVar, heat_cap_spec, hIn_SA, hOut_SA,
  htCap_Liq, htCap_W, ht_flux, ht_flux_C, ht_flux_in, ht_flux_out, ht_gen_vol,
  ht_xfer_co, ht_xfer_CW, mass, tank_D, tank_L, tank_V, temp, temp_coil, temp_diff, temp_env,
  temp_init, temp_water, thFluxVect, time, time_final, vol, water_dense, water_m,
  water_vol]

coil_SA, dummyVar, hIn_SA, hOut_SA, htCap_Liq, htCap_W, ht_flux_C,
  ht_flux_in, ht_flux_out, ht_gen_vol, ht_xfer_co, ht_xfer_CW, tank_D, tank_L, tank_V,
  temp_coil, temp_diff, temp_env, temp_init, temp_water, thFluxVect, time_final,
  water_dense, water_m, water_vol :: UnitalChunk

-- convenience

coil_SA     = uc' "coil_SA" (compoundPhrase (nounPhrase'' (phrase $ coil ^. term) (phrase $ coil ^. term) CapFirst CapWords) (nounPhrase'' (phrase $ surArea ^. term) (phrase $ surArea ^. term) CapFirst CapWords))
              "Area covered by the outermost layer of the coil" (sub cA cC) m_2
dummyVar    = uc' "dummyVar" 
              (nounPhraseSP "dummy variable for integration over time")
              "Binary value representing the presence or absence of integration over time" (Greek Tau_L) second
hIn_SA      = uc' "hIn_SA" 
              (nounPhraseSP "surface area over which heat is transferred in")
              "Surface area over which thermal energy is transferred into an object" (sub cA (Atomic "in")) m_2
hOut_SA     = uc' "hOut_SA" 
              (nounPhraseSP "surface area over which heat is transferred out")
              "Surface area over which thermal energy is transferred out of an object" (sub cA (Atomic "out")) m_2
htCap_Liq   = uc' "htCap_Liq" (nounPhraseSP "specific heat capacity of a liquid")
              "The amount of energy required to raise the temperature of a given unit mass of a given liquid by a given amount"  (sup cC cL) U.heat_cap_spec
htCap_W     = uc' "htCap_W" (heat_cap_spec `of_` water)
              "The amount of energy required to raise the temperature of a given unit mass of water by a given amount" (sub cC cW) U.heat_cap_spec
ht_flux_C   = uc' "ht_flux_C" (nounPhraseSP "heat flux from coil")
              "The rate of thermal energy transfer through the coil per unit time" (sub lQ cC) U.thermal_flux
ht_flux_in  = uc' "ht_flux_in" (nounPhraseSP "heat flux in")
              "The rate of thermal energy transfer into an object through a given surface per unit time" (sub lQ (Atomic "in")) U.thermal_flux
ht_flux_out = uc' "ht_flux_out" (nounPhraseSP "heat flux out")
              "The rate of thermal energy transfer out of an object through a given surface per unit time" (sub lQ (Atomic "out")) U.thermal_flux
ht_gen_vol  = uc' "ht_gen_vol" 
              (nounPhraseSP "volumetric heat generation per unit volume")
              "Amount of thermal energy generated per unit volume" lG U.thermal_flux
ht_xfer_co  = uc' "ht_xfer_co" 
              (nounPhraseSP "convective heat transfer coefficient")
              "The proportionality constant between the heat flux and the thermodynamic driving force for the flow of thermal energy" lH U.heat_transfer_coef
ht_xfer_CW  = uc' "ht_xfer_CW" 
              (nounPhraseSP "convective heat transfer coefficient between coil and water")
              ("The convective heat transfer coefficient that models " ++
              "the thermal flux from the coil to the surrounding water") (sub lH cC) U.heat_transfer_coef
  -- How do I make a symbol that needs one (or more) Accent? Add to Symbol or
  -- pull Accent out somehow?
tank_D      = uc' "tank_D" (diameter `of_` tank) "The diameter of the tank" cD metre
tank_L      = uc' "tank_L" (len `of_` tank) "The length of the tank" cL metre
tank_V      = uc' "V_tank" (nounPhraseSP "volume of the cylindrical tank") "The amount of space encompassed by the tank" (sub cV (Atomic "tank")) m_3
temp_coil   = uc' "temp_coil" (temp `of_` coil)
              "The average kinetic energy of the particles within the coil" (sub cT cC) centigrade
temp_diff   = uc' "temp_diff" (nounPhraseSP "temperature difference")
              "Measure of the relative amounts of internal energy within two bodies" (Concat [Greek Delta, cT]) centigrade
temp_env    = uc' "temp_env" (temp `of_` environment)
              "The tempature of a given environment" (sub cT (Atomic "env")) centigrade
thFluxVect  = uc' "thFluxVect" (cn' "thermal flux vector")
              "Vector denoting the direction of thermal flux through a surface" (vec lQ) U.thermal_flux
time_final  = uc' "time_final" (cn' "time")
              ("The amount of time elapsed from the beginning of the" ++
              " simulation to its conclusion") (sub lT (Atomic "final")) second
temp_init   = uc' "temp_init" (cn' "initial temperature")
              "The temperature at the beginning of the simulation" (sub cT (Atomic "init")) centigrade
temp_water  = uc' "temp_water" (temp `of_` water)
              "The average kinetic energy of the particles within the water" (sub cT cW) centigrade
water_dense = uc' "water_dense" (density `of_` water)
              "The amount of mass per unit volume of water" (sub (Greek Rho_L) cW) densityU
water_m     = uc' "water_m" (mass `of_` water)
              "The amount of matter within a given quantity of water" (sub (mass ^. symbol) cW) kilogram
water_vol   = uc' "water_vol" (vol `of_` water)
              "The amount of space occupied by a given quantity of water" (sub cV cW) m_3
--melt_frac   = uc' "Phi" "melt fraction" (Greek Phi) unitless

--Common Terms
coil, tank, water, ht_trans :: NPNC

coil        = npnc "coil"           (cn' "coil")
tank        = npnc "tank"           (cn' "tank")
water       = npnc "water"          (cn "water")

ht_trans    = npnc "heat transfer"  (cn "heat transfer") --Not really a nounphase, just a hack to get RefSec to work

----Acronyms-----
acronyms :: [CINP]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ode,
            physSyst,requirement,srs,sWHS,thModel]
  
sWHS, sWHT :: CINP

sWHS  = commonINP "sWHS"  (cn' "solar water heating system")  "SWHS"
sWHT  = commonINP "sWHT"  (cn' "solar water heating tank")    "SWHT"  

----EqChunks----
--Theoretical models--
t1consThermE :: RelationConcept
t1consThermE = makeRC "t1consThermE" 
  (nounPhraseSP "Conservation of thermal energy")
  t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) := 
  (C density) * (C heat_cap_spec) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = 
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+: 
  (P $ heat_cap_spec ^. symbol) :+: S " and density " :+: (P $ density ^. symbol) :+:
  S ", where " :+: (P $ thFluxVect ^. symbol) +:+ S "is the thermal flux vector," +:+
  (P $ ht_gen_vol ^. symbol) +:+ S "is the volumetric heat generation," +:+
  (P $ temp ^. symbol) +:+ S "is the temperature," +:+ (P $ time ^. symbol) +:+
  S "is time, and" +:+ (P $ gradient ^. symbol) +:+ S "is the gradient operator."
  +:+ S "For this equation to apply, other forms of energy, such as mechanical"
  +:+ S "energy, are assumed to be negligible in the") --FIXME: Add 'system (A1).' or Assumption 1 reference
  
srs_swhs :: ConceptChunk -- Used to make the title of the paper
  
srs_swhs = dcc "srs_swhs" (nounPhraseSP 
  "Solar Water Heating Systems")
  "SWHS"