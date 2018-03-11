module Drasil.GlassBR.DataDefs (dataDefns, gbQDefns, 
  risk, hFromt, strDisFac, nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac) where

import Language.Drasil

import Prelude hiding (log, exp)
import Drasil.GlassBR.Unitals (tolLoad, dimlessLoad, gTF, stressDistFac, 
  aspectR, aspectRWithEqn, demand, sdf_tol, nom_thick, act_thick, pb_tol,
  plate_width, plate_len, sflawParamM, mod_elas, glass_type, sflawParamK,
  glassTypeFactors, lDurFac, glassTypeAbbrsStr, nonFactorL, 
  actualThicknesses, nominalThicknesses, risk_fun)

import Data.Drasil.Utils (getES, mkDataDef', mkDataDef)
import Data.Drasil.SentenceStructures (sAnd)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Math (probability, parameter, calculation)
import Data.Drasil.Concepts.Documentation (datum, user)

import Control.Lens ((^.))

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefns :: [QDefinition]
dataDefns = [risk, hFromt, strDisFac, nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac]

gbQDefns :: [Block QDefinition]
gbQDefns = [Parallel hFromt {-DD2-} [glaTyFac {-DD6-}]] ++ --can be calculated on their own
  map (\x -> Parallel x []) [dimLL {-DD7-}, strDisFac {-DD4-}, risk {-DD1-},
  tolStrDisFac {-DD9-}, tolPre {-DD8-}, nonFL {-DD5-}] 

--DD1--

risk_eq :: Expr
risk_eq = ((sy sflawParamK) / 
  ((sy plate_len) * (sy plate_width)) $^ ((sy sflawParamM) - 1) *
  (1000 * sy mod_elas * (square $ sy act_thick)) $^ (sy sflawParamM) 
  * (sy lDurFac) * (exp (sy stressDistFac)))

risk :: QDefinition
risk = aqd (mkDataDef' risk_fun risk_eq (aGrtrThanB +:+ hRef +:+ ldfRef +:+ jRef))
  [SourceRef $ S "[4]"]

--DD2--

hFromt_eq :: Relation
hFromt_eq = (1/1000) * (case_ (zipWith hFromt_helper 
  actualThicknesses nominalThicknesses))

hFromt_helper :: Double -> Double -> (Expr, Relation)
hFromt_helper result condition = (dbl result, (sy nom_thick) $= dbl condition)

hFromt :: QDefinition
hFromt = aqd (mkDataDef' act_thick hFromt_eq (hMin)) ([] :: Attributes)

--DD3--

-- loadDF_eq :: Expr 
-- loadDF_eq = (sy load_dur / 60) $^ (sy sflawParamM / 16)

-- loadDF :: QDefinition
-- loadDF = mkDataDef lDurFac loadDF_eq

--DD4--

strDisFac_eq :: Expr
strDisFac_eq = FCall (sy stressDistFac) 
  [sy dimlessLoad, (sy plate_len) / (sy plate_width)]
--strDisFac_eq = FCall (asExpr interpZ) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy dimlessLoad]
  
strDisFac :: QDefinition
strDisFac = aqd (mkDataDef' stressDistFac strDisFac_eq
  (jRef2 +:+ qHtRef +:+ aGrtrThanB)) ([] :: Attributes)

--DD5--

nonFL_eq :: Expr
nonFL_eq = ((sy tolLoad) * (sy mod_elas) * (sy act_thick) $^ 4) /
  (square (sy plate_len * sy plate_width))

nonFL :: QDefinition
nonFL = aqd (mkDataDef' nonFactorL nonFL_eq (aGrtrThanB +:+ hRef +:+ qHtTlTolRef))
  ([] :: Attributes)

--DD6--

glaTyFac_eq :: Expr
glaTyFac_eq = (case_ (zipWith glaTyFac_helper glassTypeFactors glassTypeAbbrsStr))

glaTyFac_helper :: Integer -> String -> (Expr, Relation)
glaTyFac_helper result condition = (int result, (sy glass_type) $= str condition)

glaTyFac :: QDefinition
glaTyFac = aqd (mkDataDef gTF glaTyFac_eq) ([] :: Attributes)

--DD7--

dimLL_eq :: Expr
dimLL_eq = ((sy demand) * (square (sy plate_len * sy plate_width)))
  / ((sy mod_elas) * (sy act_thick $^ 4) * (sy gTF))

dimLL :: QDefinition
dimLL = aqd (mkDataDef' dimlessLoad dimLL_eq 
  (qRef +:+ aGrtrThanB +:+ hRef +:+ gtfRef)) ([] :: Attributes)

--DD8--

tolPre_eq :: Expr
tolPre_eq = FCall (sy tolLoad) [sy sdf_tol, (sy plate_len) / (sy plate_width)]
--tolPre_eq = FCall (asExpr interpY) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy sdf_tol]

tolPre :: QDefinition
tolPre = aqd (mkDataDef' tolLoad tolPre_eq (qHtTlExtra)) ([] :: Attributes)

--DD9--

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = log (log (1 / (1 - (sy pb_tol)))
  * ((((sy plate_len) * (sy plate_width)) $^ (sy sflawParamM - 1) / 
    ((sy sflawParamK) * ((1000 * sy mod_elas *
    (square (sy act_thick)))) $^ (sy sflawParamM) * (sy lDurFac)))))

tolStrDisFac :: QDefinition
tolStrDisFac = aqd (mkDataDef' sdf_tol tolStrDisFac_eq
  (aGrtrThanB +:+ hRef +:+ ldfRef +:+ pbTolUsr)) ([] :: Attributes)

--Issue #350

aGrtrThanB :: Sentence
aGrtrThanB = ((getES plate_len) `sC` (getES plate_width) +:+ 
  S "are" +:+ plural dimension +:+ S "of the plate" `sC` S "where" +:+. 
  sParen (E (sy plate_len $> sy plate_width)))

hRef :: Sentence
hRef = (getES nom_thick +:+ S "is the true thickness" `sC` 
  S "which is based on the nominal thicknesses" +:+. S "as shown in DD2")

ldfRef :: Sentence
ldfRef = (getES lDurFac +:+ S "is the" +:+ phrase lDurFac +:+. 
  S "as defined by DD3")

pbTolUsr :: Sentence
pbTolUsr = (getES pb_tol +:+ S "is the tolerable" +:+ phrase probability 
  +:+ S "entered by the" +:+. phrase user)

jRef :: Sentence
jRef = (getES stressDistFac +:+ S "is the" +:+ phrase stressDistFac +:+.
  S ", as defined in DD4")

hMin :: Sentence
hMin = (getES nom_thick +:+ S "is a function that maps from the nominal thickness"
  +:+ sParen (getES act_thick) +:+. S "to the minimum thickness")

qHtTlExtra :: Sentence
qHtTlExtra = (getES tolLoad +:+ S "is the tolerable pressure which is obtained from Figure 7 using" 
  +:+ getES sdf_tol `sAnd` phrase aspectR +:+ sParen (E $ aspectRWithEqn^.equat) +:+
  S "as" +:+ plural parameter +:+. S "using interpolation" +:+ titleize calculation +:+
  S "of" +:+ getES sdf_tol +:+. S "is defined in DD9")

qHtTlTolRef :: Sentence
qHtTlTolRef = (getES tolLoad +:+. S "is the tolerable pressure defined in DD8")

qRef :: Sentence
qRef = (getES demand +:+. S "is the 3 second equivalent pressure, as given in IM3")

gtfRef :: Sentence
gtfRef = (getES gTF +:+ S "is the" +:+. (phrase gTF `sC` S "as given by DD6"))

qHtRef :: Sentence
qHtRef = (getES dimlessLoad +:+ S "is the" +:+ phrase dimlessLoad +:+.
  S "defined in DD7")

jRef2 :: Sentence
jRef2 = (getES stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` 
  S "which is obtained by" +:+ S "interpolating from" +:+ plural datum +:+. 
  S "shown in Figure 7")
