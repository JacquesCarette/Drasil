module Drasil.HGHC.HeatTransfer where

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics

varChunks :: [VarChunk]
varChunks = [tau_c, h_b, h_p, k_c]

--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: VarChunk
tau_c = makeVC "tau_c" "clad thickness" ((Greek Tau_L) `sub` lC)

--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}
--------------- --------------- --------------- ---------------
h_c_eq :: Expr
h_c_eq = --UnaryOp (Summation (Just 
  -- (Low ((makeVC "i" "" lI),0), High (C (makeVC "n" "" lN)))))
  (2 * (C k_c) * (C h_b) / (2 * (C k_c) + (C tau_c) * (C h_b)))

h_c :: QDefinition
h_c = fromEqn "h_c" (S 
  "convective heat transfer coefficient between clad and coolant")
  (lH `sub` lC) heat_transfer h_c_eq

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_g ---------------}
-- --------------- --------------- --------------- ---------------
h_g_eq :: Expr
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

h_g :: QDefinition
h_g = fromEqn "h_g" (S
  "effective heat transfer coefficient between clad and fuel surface")
  (lH `sub` lG) heat_transfer h_g_eq

--------------- --------------- --------------- ---------------
{--------------- Begin h_b ---------------}
--------------- --------------- --------------- ---------------

h_b :: VarChunk
h_b = makeVC "h_b" "initial coolant film conductance" (lH `sub` lB)

--------------- --------------- --------------- ---------------
{--------------- Begin h_p ---------------}
--------------- --------------- --------------- ---------------

h_p :: VarChunk
h_p = makeVC "h_p" "initial gap film conductance" (lH `sub` lP)

--------------- --------------- --------------- ---------------
{--------------- Begin k_c ---------------}
--------------- --------------- --------------- ---------------

k_c :: VarChunk
k_c = makeVC "k_c" "clad conductivity" (lK `sub` lC)
