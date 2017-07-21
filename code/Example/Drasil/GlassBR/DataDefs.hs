module Drasil.GlassBR.DataDefs where

import Language.Drasil
import Prelude hiding (log, id, exp, sqrt)
import Drasil.GlassBR.Unitals
import Data.Drasil.Utils
import Data.Drasil.SentenceStructures (extrctStrng, sAnd)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Math (probability, parameter, calculation)
import Data.Drasil.Concepts.Documentation (datum)

--FIXME: having id "" and term "" is completely bogus, and should not
--  be allowed.  This implicitly says that something here does not make sense.

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefns :: [QDefinition]
dataDefns = [risk, hFromt, loadDF, strDisFac, nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac]

gbQDefns :: [Block QDefinition]
gbQDefns = [Parallel hFromt {-DD2-} [loadDF {-DD3-}, glaTyFac {-DD6-}]] ++ --can be calculated on their own
  map (\x -> Parallel x []) [dimLL {-DD7-}, strDisFac {-DD4-}, risk {-DD1-},
  tolStrDisFac {-DD9-}, tolPre {-DD8-}, nonFL {-DD5-}] 

--DD1--

--Source : #7 -> See Issue #357

risk_eq :: Expr
risk_eq = ((C sflawParamK) / (Grouping (((C plate_len) / (1000)) *
  ((C plate_width) / (1000)))) :^ ((C sflawParamM) - (1))) *
  (Grouping ((Grouping ((C mod_elas) * (1000))) * 
  (square (Grouping ((C act_thick) / (1000)))))) :^ (C sflawParamM) * 
  (C loadDF) * (exp (C stressDistFac))

risk :: QDefinition
risk = mkDataDef' risk_fun risk_eq (aGrtrThanB +:+ hRef +:+ ldfRef +:+ jRef)

--DD2--

hFromt_eq :: Relation
hFromt_eq = (Case (zipWith hFromt_helper actualThicknesses nominalThicknesses))

hFromt_helper :: Double -> Double -> (Expr, Relation)
hFromt_helper result condition = (Dbl result, (C nom_thick) := Dbl condition)

hFromt :: QDefinition
hFromt = mkDataDef act_thick hFromt_eq

--DD3--

loadDF_eq :: Expr 
loadDF_eq = (Grouping ((C load_dur) / (60))) :^ ((C sflawParamM) / (16))

--FIXME: Should we be using id here? My gut says no, but I'll look in 
-- more depth shortly.
-- Definitely should not have the id being printed (which it currently is)
loadDF :: QDefinition
loadDF = mkDataDef lDurFac loadDF_eq

--DD4--

strDisFac_eq :: Expr
strDisFac_eq = FCall (C stressDistFac) 
  [C dimlessLoad, (C plate_len) / (C plate_width)]

strDisFac :: QDefinition
strDisFac = mkDataDef' stressDistFac strDisFac_eq (jRef2 +:+ qHtRef +:+ aGrtrThanB)

--DD5--

nonFL_eq :: Expr
nonFL_eq = ((C tolLoad) * (C mod_elas) * (C act_thick) :^ (4)) /
  (square (Grouping ((C plate_len) * (C plate_width))))

nonFL :: QDefinition
nonFL = mkDataDef' nonFactorL nonFL_eq (aGrtrThanB +:+ hRef +:+ qHtTlTolRef)

--DD6--

glaTyFac_eq :: Expr
glaTyFac_eq = (Case (zipWith glaTyFac_helper glassTypeFactors (map extrctStrng glassTypeAbbrs)))

glaTyFac_helper :: Integer -> String -> (Expr, Relation)
glaTyFac_helper result condition = (Int result, (C glass_type) := V condition)

glaTyFac :: QDefinition
glaTyFac = mkDataDef gTF glaTyFac_eq

--DD7--

dimLL_eq :: Expr
dimLL_eq = ((C demand) * (square (Grouping ((C plate_len) * (C plate_width)))))
  / ((C mod_elas) * ((C act_thick) :^ (4)) * (C gTF))

dimLL :: QDefinition
dimLL = mkDataDef' dimlessLoad dimLL_eq (qRef +:+ aGrtrThanB +:+ hRef +:+ gtfRef)

--DD8--

tolPre_eq :: Expr
tolPre_eq = FCall (C tolLoad) [C sdf_tol, (C plate_len) / (C plate_width)]

tolPre :: QDefinition
tolPre = mkDataDef tolLoad tolPre_eq

--DD9--

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = log (log ((1) / ((1) - (C pb_tol)))
  * ((Grouping (((C plate_len) / (1000)) * ((C plate_width) / (1000)))) :^
  ((C sflawParamM) - (1)) / ((C sflawParamK) *
  (Grouping (Grouping ((C mod_elas) * (1000)) *
  (square (Grouping ((C act_thick) / (1000))))
  )) :^ (C sflawParamM) * (C loadDF))))

tolStrDisFac :: QDefinition
tolStrDisFac = mkDataDef' sdf_tol tolStrDisFac_eq (aGrtrThanB +:+ hRef +:+ ldfRef +:+ pbTolUsr)

--Issue #350

aGrtrThanB :: Sentence
aGrtrThanB = ((getS plate_len) `sC` (getS plate_width) +:+ S "are" +:+ plural dimension +:+
   S "of the plate" `sC` S "where" +:+. sParen (E (C plate_len :> C plate_width)))

hRef :: Sentence
hRef = (getS nom_thick +:+ S "is the true thickness" `sC` S "which is based on the nominal thicknesses"
  +:+. S "as shown in DD2")

ldfRef :: Sentence
ldfRef = (getS loadDF +:+ S "is the" +:+ phrase loadDF +:+. S "as defined by DD3")

pbTolUsr :: Sentence
pbTolUsr = (getS pb_tol +:+ S "is the tolerable" +:+ phrase probability +:+. S "entered by the user")

jRef :: Sentence
jRef = (getS stressDistFac +:+ S "is the" +:+ phrase stressDistFac +:+. S ", as defined in DD4")

hMin :: Sentence
hMin = (getS nom_thick +:+ S "is a function that maps from the nominal thickness" +:+ 
  sParen (getS act_thick) +:+. S "to the minimum thickness")

qHtTlExtra :: Sentence
qHtTlExtra = (getS tolLoad +:+ S "is the tolerable pressure which is obtained from Figure 7 using" 
  +:+ getS sdf_tol `sAnd` phrase aspectR +:+ sParen (E (equat aspectRWithEqn)) +:+
  S "as" +:+ plural parameter +:+. S "using interpolation" +:+ titleize calculation +:+
  S "of" +:+ getS sdf_tol +:+. S "is defined in DD9")

qHtTlTolRef :: Sentence
qHtTlTolRef = (getS tolLoad +:+. S "is the tolerable pressure defined in DD8")

qRef :: Sentence
qRef = (getS demand +:+. S "is the 3 second equivalent pressure, as given in IM3")

gtfRef :: Sentence
gtfRef = (getS gTF +:+ S "is the" +:+. (phrase gTF `sC` S "as given by DD6"))

qHtRef :: Sentence
qHtRef = (getS dimlessLoad +:+ S "is the" +:+ phrase dimlessLoad +:+. S "defined in DD7")

jRef2 :: Sentence
jRef2 = (getS stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` S "which is obtained by"
  +:+ S "interpolating from" +:+ plural datum +:+. S "shown in Figure 7")