{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.HeatTransfer where
import Language.Drasil.Expr (Expr(..))
import Language.Drasil.SI_Units
import Language.Drasil.Unicode (Tau(..))
import Language.Drasil.Unit (Unit(..), UDefn(..), DerUChunk(..), FundUnit(..),
  from_udefn)
import Language.Drasil.Chunk (VarChunk(..), ConceptChunk(..), makeCC, makeVC)
import Language.Drasil.Chunk.Eq (EqChunk(..), fromEqn)
import Language.Drasil.Symbol
import Language.Drasil.Spec (USymb(..),Sentence(..))

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

h,c :: Symbol
h = Atomic "h"
c = Atomic "c"
--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: VarChunk
tau_c = makeVC "tau_c" "clad thickness" (sub (Special Tau_L) c)

--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}
--------------- --------------- --------------- ---------------
h_c_eq :: Expr
h_c_eq = 2 * (C k_c) * (C h_b) / (2 * (C k_c) + (C tau_c) * (C h_b))

h_c :: EqChunk
h_c = fromEqn "h_c" (S 
  "convective heat transfer coefficient between clad and coolant")
  (sub h c) heat_transfer h_c_eq

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_g ---------------}
-- --------------- --------------- --------------- ---------------
h_g_eq :: Expr
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

h_g :: EqChunk
h_g = fromEqn "h_g" (S
  "effective heat transfer coefficient between clad and fuel surface")
  (sub h (Atomic "g")) heat_transfer h_g_eq

--------------- --------------- --------------- ---------------
{--------------- Begin h_b ---------------}
--------------- --------------- --------------- ---------------

h_b :: VarChunk
h_b = makeVC "h_b" "initial coolant film conductance" (sub h (Atomic "b"))

--------------- --------------- --------------- ---------------
{--------------- Begin h_p ---------------}
--------------- --------------- --------------- ---------------

h_p :: VarChunk
h_p = makeVC "h_p" "initial gap film conductance" (sub h (Atomic "p"))

--------------- --------------- --------------- ---------------
{--------------- Begin k_c ---------------}
--------------- --------------- --------------- ---------------

k_c :: VarChunk
k_c = makeVC "k_c" "clad conductivity" (sub h c)
