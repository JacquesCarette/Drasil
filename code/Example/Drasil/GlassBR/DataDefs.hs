module Drasil.GlassBR.DataDefs where

import Language.Drasil
import Data.Drasil.SI_Units
import Prelude hiding (log, id)
import Control.Lens ((^.))
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.Concepts


dataDefns :: [QDefinition]
dataDefns = [risk, hFromt, loadDF, strDisFac, nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac]

risk :: QDefinition
risk = fromEqn' (risk_fun ^. id) (nounPhraseSP "risk of failure") 
  (risk_fun ^. symbol) risk_eq

risk_eq :: Expr
risk_eq = ((C sflawParamK):/(Grouping (((C plate_len):/(Int 1000)):*
  ((C plate_width):/(Int 1000)))):^((C sflawParamM) - (Int 1))):*
  (Grouping ((Grouping ((C mod_elas):*(Int 1000))):*(Grouping ((C act_thick)
  :/(Int 1000))):^(Int 2))):^(C sflawParamM):*(C loadDF):*(V "e"):^(C sdf)

hFromt_eq :: Expr
hFromt_eq = FCall (C act_thick) [C nom_thick]

hFromt :: QDefinition
hFromt = fromEqn (act_thick ^. id) 
  (nounPhraseSP $ " function that maps from the " ++
  "nominal thickness (t) to the " ++
  "minimum thickness, as follows: h(t) = (t = 2.5 => 2.16 | t = 2.7 => " ++
  "2.59 | t = 3.0 => 2.92 | t = 4.0 => 3.78 | t = 5.0 => 4.57 | t = 6.0 " ++
  "=> 5.56 | t = 8.0 => 7.42 | t = 10.0 => 9.02 | t = 12.0 => 11.91 | " ++
  "t = 16.0 => 15.09 | t = 19.0 => 18.26 | t = 22.0 => 21.44)")
  (act_thick ^.symbol) millimetre hFromt_eq

loadDF_eq :: Expr 
loadDF_eq = (Grouping ((C load_dur):/(Int 60))):^((C sflawParamM):/(Int 16))

--FIXME: Should we be using id here? My gut says no, but I'll look in 
-- more depth shortly.
-- Definitely should not have the id being printed (which it currently is)
loadDF :: QDefinition
loadDF = fromEqn' (lDurFac ^. id) (lDurFac ^. term) (Atomic "LDF") loadDF_eq

strDisFac_eq :: Expr
strDisFac_eq = FCall (C sdf) [C dimlessLoad, (C plate_len):/(C plate_width)]

strDisFac :: QDefinition
strDisFac = fromEqn' (sdf ^. id) (sdf ^. term) (sdf ^. symbol) 
  strDisFac_eq

nonFL_eq :: Expr
nonFL_eq = ((C tolLoad):*(C mod_elas):*(C act_thick):^(Int 4)):/
  ((Grouping ((C plate_len):*(C plate_width))):^(Int 2))

nonFL :: QDefinition
nonFL = fromEqn' (nonFactorL ^. id) (nonFactorL ^. term) (Atomic "NFL") nonFL_eq

glaTyFac_eq :: Expr
glaTyFac_eq = FCall (C glaTyFac) [C glass_type]

glaTyFac :: QDefinition
glaTyFac = fromEqn' (glassTypeFac ^. id) (nounPhraseSP $ 
  "function that maps from " ++ "the glass type (g) to a real " ++
  "number, as follows: GTF(g) = (g = AN => 1.0|g = FT => 4.0|" ++ 
  "g = HS => 2.0). AN is annealed glass. " ++ 
  "FT is fully tempered glass. HS is heat strengthened glass.") (Atomic "GTF") 
  glaTyFac_eq

dimLL_eq :: Expr
dimLL_eq = ((C demand):*((Grouping ((C plate_len):*(C plate_width))):^(Int 2)))
  :/((C mod_elas):*((C act_thick):^(Int 4)):*(C gTF))

dimLL :: QDefinition
dimLL = fromEqn' (dimlessLoad ^. id) (dimlessLoad ^. term) 
  (dimlessLoad ^. symbol) dimLL_eq

tolPre_eq :: Expr
tolPre_eq = FCall (C tolLoad) [C sdf_tol, (C plate_len):/(C plate_width)]

tolPre :: QDefinition
tolPre = fromEqn' (tolLoad ^. id) (tolLoad ^. term) (tolLoad ^. symbol) 
  tolPre_eq

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = log (log ((Int 1):/((Int 1):-(C pb_tol)))
  :*((Grouping (((C plate_len):/(Int 1000)):*((C plate_width):/(Int 1000)))):^
  ((C sflawParamM) :- (Int 1)):/((C sflawParamK):*
  (Grouping (Grouping ((C mod_elas):*(Int 1000)):*
  (Grouping ((C act_thick):/(Int 1000))):^
  (Int 2))):^(C sflawParamM):*(C loadDF))))

tolStrDisFac :: QDefinition
tolStrDisFac = fromEqn' (sdf_tol ^. id) (sdf_tol ^. term) (sdf_tol ^. symbol) 
  tolStrDisFac_eq