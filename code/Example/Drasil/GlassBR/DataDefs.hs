module Drasil.GlassBR.DataDefs where

import Language.Drasil
import Data.Drasil.SI_Units
import Prelude hiding (log, id, exp)
import Control.Lens ((^.))
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.Concepts

--FIXME: having id "" and term "" is completely bogus, and should not
--  be allowed.  This implicitly says that something here does not make sense.

dataDefns :: [QDefinition]
dataDefns = [risk, hFromt, loadDF, strDisFac, nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac]

risk :: QDefinition
risk = fromEqn' (risk_fun ^. id) (nounPhraseSP "risk of failure") 
  (risk_fun ^. symbol) risk_eq

risk_eq :: Expr
risk_eq = ((C sflawParamK) :/ (Grouping (((C plate_len) :/ (Int 1000)) :*
  ((C plate_width) :/ (Int 1000)))) :^ ((C sflawParamM) - (Int 1))) :*
  (Grouping ((Grouping ((C mod_elas) :* (Int 1000))) :* 
  (Grouping ((C act_thick) :/ (Int 1000))) :^ (Int 2))) :^ (C sflawParamM) :* 
  (C loadDF) :* (exp (C stressDistFac))

hFromt_eq :: Relation
hFromt_eq = (Case (zipWith hFromt_helper
  [2.16, 2.59, 2.92, 3.78, 4.57, 5.56, 7.42, 9.02, 11.91, 15.09, 18.26, 21.44]
  [2.5, 2.7, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0]))

hFromt_helper :: Double -> Double -> (Expr, Relation)
hFromt_helper result condition = (Dbl result, (C nom_thick) := Dbl condition)

hFromt :: QDefinition
hFromt = fromEqn (act_thick ^. id) 
  (nounPhraseSP $ "h is the function that maps from the nominal thickness (t) to " ++
  "the minimum thickness. h is the actual thickness. t is the nominal thickness t " ++
  "in {2.5, 2.7, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0}")
  (act_thick ^.symbol) millimetre hFromt_eq

loadDF_eq :: Expr 
loadDF_eq = (Grouping ((C load_dur):/(Int 60))):^((C sflawParamM):/(Int 16))

--FIXME: Should we be using id here? My gut says no, but I'll look in 
-- more depth shortly.
-- Definitely should not have the id being printed (which it currently is)
loadDF :: QDefinition
loadDF = fromEqn' (lDurFac_ ^. id) (lDurFac ^. term) (Atomic "LDF") loadDF_eq

strDisFac_eq :: Expr
strDisFac_eq = FCall (C stressDistFac) [C dimlessLoad, (C plate_len):/(C plate_width)]

strDisFac :: QDefinition
strDisFac = fromEqn' (stressDistFac ^. id) (stressDistFac ^. term) (stressDistFac ^. symbol) 
  strDisFac_eq

nonFL_eq :: Expr
nonFL_eq = ((C tolLoad):*(C mod_elas):*(C act_thick):^(Int 4)):/
  ((Grouping ((C plate_len):*(C plate_width))):^(Int 2))

nonFL :: QDefinition
nonFL = fromEqn' (nonFactorL_ ^. id) (nonFactorL ^. term) (Atomic "NFL") nonFL_eq

glaTyFac_eq :: Expr
glaTyFac_eq = FCall (C glaTyFac) [C glass_type]

glaTyFac :: QDefinition --FIXME: make into cases
glaTyFac = fromEqn' (glassTypeFac_ ^. id) (nounPhraseSP $ 
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