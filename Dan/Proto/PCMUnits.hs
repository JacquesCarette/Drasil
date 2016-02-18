{-# OPTIONS -Wall #-}
module PCMUnits where

-- import ASTInternal (Expr(..))
import SI_Units
-- import Unicode (Tau(..))
import Unit (Unit(..), USymb(..), UDefn(..), DerUChunk(..), FundUnit(..),
  from_udefn, makeDerU)
import Chunk (ConceptChunk(..))
-- import EqChunk (EqChunk(..), fromEqn)
import Symbol
-- import UnitalChunk

import Control.Lens ((^.))

--m^2--
m_2 :: DerUChunk
m_2 = DUC (UD m_2con (from_udefn m_2eqn)) m_2eqn

m_2con :: ConceptChunk
m_2con = CC "square metres" "square metres"

m_2eqn :: UDefn
m_2eqn = USynonym (UPow (metre ^. unit) (2))

--J/(kg*C)--
heat_capacity :: DerUChunk
heat_capacity = DUC (UD ht_cap ht_cap_symb) heat_cap_eqn

ht_cap :: ConceptChunk
ht_cap = CC "heat capacity" "heat capacity"

ht_cap_symb :: USymb
ht_cap_symb = from_udefn heat_cap_eqn

heat_cap_eqn :: UDefn
heat_cap_eqn = USynonym (UDiv 
  (joule ^. unit) (UProd [kilogram ^. unit, centigrade ^. unit]))

--
thermFluxU :: DerUChunk
thermFluxU = makeDerU thermFluxUcon thermFluxUeqn

thermFluxUcon :: ConceptChunk
thermFluxUcon = CC "flux" "flux"

thermFluxUeqn :: UDefn
thermFluxUeqn = USynonym (UDiv (watt ^. unit) (m_2 ^. unit))

--W/(m^2C)--  
heat_transfer :: DerUChunk
heat_transfer = DUC (UD ht_con ht_symb) heat_transfer_eqn

ht_con :: ConceptChunk
ht_con = CC "Heat transfer" "Heat transfer"

ht_symb :: USymb
ht_symb = from_udefn heat_transfer_eqn

heat_transfer_eqn :: UDefn
heat_transfer_eqn = USynonym (UProd 
  [kilogram ^. unit, UPow (second ^. unit) (-3),
   UPow (centigrade ^. unit) (-1)])

h,c :: Symbol
h = Atomic "h"
c = Atomic "c"
