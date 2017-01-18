module Drasil.HGHC.HeatTransfer where

import Language.Drasil

import Data.Drasil.Units.Thermodynamics (heat_transfer_coef)

varChunks :: [VarChunk]
varChunks = [cladThick, coolFilmCond, gapFilmCond, cladCond]

--------------- --------------- --------------- ---------------
{--------------- Begin cladThick ---------------}
--------------- --------------- --------------- ---------------
cladThick :: VarChunk
cladThick = makeVC "cladThick" "clad thickness" ((Greek Tau_L) `sub` lC)

--------------- --------------- --------------- ---------------
{--------------- Begin htTransCladCool ---------------}
--------------- --------------- --------------- ---------------
htTransCladCool_eq :: Expr
htTransCladCool_eq = --UnaryOp (Summation (Just 
  -- (Low ((makeVC "i" "" lI),0), High (C (makeVC "n" "" lN)))))
  (2 * (C cladCond) * (C coolFilmCond) / (2 * (C cladCond) + (C cladThick) * (C coolFilmCond)))

htTransCladCool :: QDefinition
htTransCladCool = fromEqn "htTransCladCool" (S 
  "convective heat transfer coefficient between clad and coolant")
  (lH `sub` lC) heat_transfer_coef htTransCladCool_eq

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin htTransCladFuel ---------------}
-- --------------- --------------- --------------- ---------------
htTransCladFuel_eq :: Expr
htTransCladFuel_eq = ((Int 2):*(C cladCond):*(C gapFilmCond)) :/ ((Int 2):*(C cladCond):+((C cladThick):*(C gapFilmCond)))

htTransCladFuel :: QDefinition
htTransCladFuel = fromEqn "htTransCladFuel" (S
  "effective heat transfer coefficient between clad and fuel surface")
  (lH `sub` lG) heat_transfer_coef htTransCladFuel_eq

--------------- --------------- --------------- ---------------
{--------------- Begin coolFilmCond ---------------}
--------------- --------------- --------------- ---------------

coolFilmCond :: VarChunk
coolFilmCond = makeVC "coolFilmCond" "initial coolant film conductance" (lH `sub` lB)

--------------- --------------- --------------- ---------------
{--------------- Begin gapFilmCond ---------------}
--------------- --------------- --------------- ---------------

gapFilmCond :: VarChunk
gapFilmCond = makeVC "gapFilmCond" "initial gap film conductance" (lH `sub` lP)

--------------- --------------- --------------- ---------------
{--------------- Begin cladCond ---------------}
--------------- --------------- --------------- ---------------

cladCond :: VarChunk
cladCond = makeVC "cladCond" "clad conductivity" (lK `sub` lC)
