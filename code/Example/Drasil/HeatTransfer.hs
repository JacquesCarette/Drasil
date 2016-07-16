module Example.Drasil.HeatTransfer where

import Language.Drasil
import Language.Drasil.SI_Units
import Language.Drasil.SymbolAlphabet

import Control.Lens ((^.))

-----
-- Need some derived units.  For now, put them here, but need to think
-- about where they really ought to go.
heat_transfer :: DerUChunk
heat_transfer = DUC (UD ht_con ht_symb) heat_transfer_eqn

ht_con :: ConceptChunk
ht_con = makeCC "Heat transfer" "Heat transfer"

ht_symb :: USymb
ht_symb = from_udefn heat_transfer_eqn

heat_transfer_eqn :: UDefn
heat_transfer_eqn = USynonym (UProd 
  [kilogram ^. unit, UPow (second ^. unit) (-3),
   UPow (centigrade ^. unit) (-1)])

--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: VarChunk
tau_c = makeVC "tau_c" "clad thickness" ((Special Tau_L) `sub` lC)

--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}
--------------- --------------- --------------- ---------------
h_c_eq :: Expr
h_c_eq = 2 * (C k_c) * (C h_b) / (2 * (C k_c) + (C tau_c) * (C h_b))

h_c :: EqChunk
h_c = fromEqn "h_c" (S 
  "convective heat transfer coefficient between clad and coolant")
  (lH `sub` lC) heat_transfer h_c_eq

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_g ---------------}
-- --------------- --------------- --------------- ---------------
h_g_eq :: Expr
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

h_g :: EqChunk
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



meth_h_g, meth_h_c :: MethodChunk
meth_h_g = fromEC h_g
meth_h_c = fromEC h_c

mod_calc :: ModuleChunk
mod_calc = makeModule "calc" (S "Calculates heat transfer coefficients")
  (S "The equations used to calculate heat transfer coefficients")
  [meth_h_g, meth_h_c]