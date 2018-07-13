module Drasil.GlassBR.DataDefs (dataDefns, dimLL, gbQDefns, glaTyFac, hFromt,
  nonFL, risk, strDisFac, tolPre, tolStrDisFac) where

import Language.Drasil
import Prelude hiding (log, exp)

import Drasil.GlassBR.Unitals (act_thick, actualThicknesses, aspectR, 
  aspectRWithEqn, demand, dimlessLoad, gTF, glassTypeAbbrsStr, 
  glassTypeFactors, glass_type, lDurFac, mod_elas, nom_thick, 
  nominalThicknesses, nonFactorL, pb_tol, plate_len, plate_width, risk_fun,
  sdf_tol, sflawParamK, sflawParamM, stressDistFac, tolLoad)

import Data.Drasil.Concepts.Documentation (datum, user)
import Data.Drasil.Concepts.Math (probability, parameter, calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.SentenceStructures (sAnd)

import Control.Lens ((^.))

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefns :: [DataDefinition] 
dataDefns = [riskDD, hFromtDD, strDisFacDD, nonFLDD, glaTyFacDD, dimLLDD,
  tolPreDD, tolStrDisFacDD]

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

-- FIXME [4] !!!
risk :: QDefinition
risk = mkDataDef risk_fun risk_eq

riskDD :: DataDefinition
riskDD = mkDD risk [sourceref $ S "[4]"] [{-derivation-}] ""{-temporary-} 
  (Just $ aGrtrThanB : hRef : ldfRef : jRef : [])

--DD2--

hFromt_eq :: Relation
hFromt_eq = (1/1000) * (case_ (zipWith hFromt_helper 
  actualThicknesses nominalThicknesses))

hFromt_helper :: Double -> Double -> (Expr, Relation)
hFromt_helper result condition = (dbl result, (sy nom_thick) $= dbl condition)

hFromt :: QDefinition
hFromt = mkDataDef act_thick hFromt_eq

hFromtDD :: DataDefinition
hFromtDD = mkDD hFromt [{-references-}] [{-derivation-}] ""--temporary
  (Just $ [hMin])

--DD3-- (#749)

-- loadDF_eq :: Expr 
-- loadDF_eq = (sy load_dur / 60) $^ (sy sflawParamM / 16)

-- loadDF :: QDefinition
-- loadDF = mkDataDef lDurFac loadDF_eq

--DD4--

strDisFac_eq :: Expr
strDisFac_eq = apply (sy stressDistFac) 
  [sy dimlessLoad, (sy plate_len) / (sy plate_width)]
--strDisFac_eq = FCall (asExpr interpZ) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy dimlessLoad]
  
strDisFac :: QDefinition
strDisFac = mkDataDef stressDistFac strDisFac_eq

strDisFacDD :: DataDefinition
strDisFacDD = mkDD strDisFac [{-references-}] [{-derivation-}] ""--temporary
  (Just $ [jRef2] ++ [qHtRef] ++ [aGrtrThanB])

--DD5--

nonFL_eq :: Expr
nonFL_eq = ((sy tolLoad) * (sy mod_elas) * (sy act_thick) $^ 4) /
  (square (sy plate_len * sy plate_width))

nonFL :: QDefinition
nonFL = mkDataDef nonFactorL nonFL_eq

nonFLDD :: DataDefinition
nonFLDD = mkDD nonFL [{-references-}] [{-derivation-}] ""--temporary
  (Just $ aGrtrThanB : hRef : qHtTlTolRef : [])

--DD6--

glaTyFac_eq :: Expr
glaTyFac_eq = (case_ (zipWith glaTyFac_helper glassTypeFactors glassTypeAbbrsStr))

glaTyFac_helper :: Integer -> String -> (Expr, Relation)
glaTyFac_helper result condition = (int result, (sy glass_type) $= str condition)

glaTyFac :: QDefinition
glaTyFac = mkDataDef gTF glaTyFac_eq

glaTyFacDD :: DataDefinition
glaTyFacDD = mkDD glaTyFac [{-references-}] [{-derivation-}] ""--temporary
  Nothing

--DD7--

dimLL_eq :: Expr
dimLL_eq = ((sy demand) * (square (sy plate_len * sy plate_width)))
  / ((sy mod_elas) * (sy act_thick $^ 4) * (sy gTF))

dimLL :: QDefinition
dimLL = mkDataDef dimlessLoad dimLL_eq

dimLLDD :: DataDefinition
dimLLDD = mkDD dimLL [{-references-}] [{-derivation-}] ""--temporary
  (Just $ qRef : aGrtrThanB : hRef : gtfRef : [])

--DD8--

tolPre_eq :: Expr
tolPre_eq = apply (sy tolLoad) [sy sdf_tol, (sy plate_len) / (sy plate_width)]
--tolPre_eq = FCall (asExpr interpY) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy sdf_tol]

tolPre :: QDefinition
tolPre = mkDataDef tolLoad tolPre_eq

tolPreDD :: DataDefinition
tolPreDD = mkDD tolPre [{-references-}] [{-derivation-}] ""--temporary
  (Just $ qHtTlExtra : [])

--DD9--

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = log (log (1 / (1 - (sy pb_tol)))
  * ((((sy plate_len) * (sy plate_width)) $^ (sy sflawParamM - 1) / 
    ((sy sflawParamK) * ((1000 * sy mod_elas *
    (square (sy act_thick)))) $^ (sy sflawParamM) * (sy lDurFac)))))

tolStrDisFac :: QDefinition
tolStrDisFac = mkDataDef sdf_tol tolStrDisFac_eq

tolStrDisFacDD :: DataDefinition
tolStrDisFacDD = mkDD tolStrDisFac [{-references-}] [{-derivation-}] ""--temporary
  (Just $ jtolRelToPbtol : aGrtrThanB : hRef : ldfRef : pbTolUsr : [])

--Additional Notes--

aGrtrThanB :: Sentence
aGrtrThanB = ((ch plate_len) `sC` (ch plate_width) +:+ 
  S "are" +:+ plural dimension +:+ S "of the plate" `sC` S "where" +:+. 
  sParen (E (sy plate_len $> sy plate_width)))

hRef :: Sentence
hRef = (ch nom_thick +:+ S "is the true thickness" `sC` 
  S "which is based on the nominal thicknesses" +:+. S "as shown in DD2")

ldfRef :: Sentence
ldfRef = (ch lDurFac +:+ S "is the" +:+ phrase lDurFac +:+. 
  S "as defined by DD3")

pbTolUsr :: Sentence
pbTolUsr = (ch pb_tol +:+ S "is the tolerable" +:+ phrase probability 
  +:+ S "entered by the" +:+. phrase user)

jRef :: Sentence
jRef = (ch stressDistFac +:+ S "is the" +:+ phrase stressDistFac +:+.
  S ", as defined in DD4")

hMin :: Sentence
hMin = (ch nom_thick +:+ S "is a function that maps from the nominal thickness"
  +:+ sParen (ch act_thick) +:+. S "to the minimum thickness")

qHtTlExtra :: Sentence
qHtTlExtra = (ch tolLoad +:+ S "is the tolerable load which is obtained from Figure 7 using" 
  +:+ ch sdf_tol `sAnd` phrase aspectR +:+ sParen (E $ aspectRWithEqn^.equat) +:+
  S "as" +:+ plural parameter +:+. S "using interpolation" +:+ titleize calculation +:+
  S "of" +:+ ch sdf_tol +:+. S "is defined in DD9")

qHtTlTolRef :: Sentence
qHtTlTolRef = (ch tolLoad +:+. S "is the tolerable load defined in DD8")

qRef :: Sentence
qRef = (ch demand +:+. S "is the 3 second equivalent pressure, as given in IM3")

gtfRef :: Sentence
gtfRef = (ch gTF +:+ S "is the" +:+. (phrase gTF `sC` S "as given by DD6"))

qHtRef :: Sentence
qHtRef = (ch dimlessLoad +:+ S "is the" +:+ phrase dimlessLoad +:+.
  S "defined in DD7")

jRef2 :: Sentence
jRef2 = (ch stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` 
  S "which is obtained by" +:+ S "interpolating from" +:+ plural datum +:+. 
  S "shown in Figure 7")

jtolRelToPbtol :: Sentence
jtolRelToPbtol = (ch sdf_tol +:+ S " is calculated with reference to " +:+. ch pb_tol)
