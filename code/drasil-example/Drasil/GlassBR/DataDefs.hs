module Drasil.GlassBR.DataDefs (aspRat, dataDefns, dimLL, gbQDefns, glaTyFac, 
  hFromt, nonFL, risk, standOffDis, strDisFac, tolPre, tolStrDisFac) where

import Language.Drasil
import Prelude hiding (log, exp, sqrt)

import Drasil.GlassBR.Concepts (annealed, fullyT, heatS)
import Drasil.GlassBR.Unitals (actualThicknesses, aspect_ratio, 
  demand, dimlessLoad, gTF, glassTypeAbbrsStr, glassTypeFactors, glass_type, 
  lDurFac, load_dur, mod_elas, nom_thick, nominalThicknesses, nonFactorL, pb_tol, 
  plate_len, plate_width, risk_fun, sdf_tol, sdx, sdy, sdz, standOffDist, sflawParamK, 
  sflawParamM, stressDistFac, tolLoad, min_thick)
import Drasil.GlassBR.Assumptions (newA5)

import Data.Drasil.Concepts.Documentation (datum, user)
import Data.Drasil.Concepts.Math (probability, parameter, calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.SentenceStructures (sAnd)

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefns :: [DataDefinition] 
dataDefns = [riskDD, hFromtDD, loadDFDD, strDisFacDD, nonFLDD, glaTyFacDD, 
  dimLLDD, tolPreDD, tolStrDisFacDD, standOffDisDD, aspRatDD]

gbQDefns :: [Block QDefinition]
gbQDefns = [Parallel hFromt {-DD2-} [glaTyFac {-DD6-}]] ++ --can be calculated on their own
  map (\x -> Parallel x []) [dimLL {-DD7-}, strDisFac {-DD4-}, risk {-DD1-},
  tolStrDisFac {-DD9-}, tolPre {-DD8-}, nonFL {-DD5-}] 

--DD1--

risk_eq :: Expr
risk_eq = ((sy sflawParamK) / 
  ((sy plate_len) * (sy plate_width)) $^ ((sy sflawParamM) - 1) *
  (sy mod_elas * (square $ sy min_thick)) $^ (sy sflawParamM) 
  * (sy lDurFac) * (exp (sy stressDistFac)))

-- FIXME [4] !!!
risk :: QDefinition
risk = mkQuantDef risk_fun risk_eq

riskDD :: DataDefinition
riskDD = mkDD risk 
  [(sourceref (S "[1]")), (sourceref (S "[4, Eq. 4-5]")), (sourceref (S "[5, Eq. 14]"))] 
  [{-derivation-}] ""{-temporary-} 
  (Just $ aGrtrThanB : hRef : ldfRef : jRef : [])

--DD2--

hFromt_eq :: Relation
hFromt_eq = (1/1000) * (case_ (zipWith hFromt_helper 
  actualThicknesses nominalThicknesses))

hFromt_helper :: Double -> Double -> (Expr, Relation)
hFromt_helper result condition = (dbl result, (sy nom_thick) $= dbl condition)

hFromt :: QDefinition
hFromt = mkQuantDef min_thick hFromt_eq

hFromtDD :: DataDefinition
hFromtDD = mkDD hFromt [{-references-}] [{-derivation-}] ""--temporary
  (Just $ [hMin])

--DD3-- (#749)

loadDF_eq :: Expr 
loadDF_eq = (sy load_dur / 60) $^ (sy sflawParamM / 16)

loadDF :: QDefinition
loadDF = mkQuantDef lDurFac loadDF_eq

loadDFDD :: DataDefinition
loadDFDD = mkDD loadDF [{-references-}] [{-derivation-}] ""--temporary
  Nothing

--DD4--

strDisFac_eq :: Expr
strDisFac_eq = apply (sy stressDistFac) 
  [sy dimlessLoad, sy aspect_ratio]
--strDisFac_eq = FCall (asExpr interpZ) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy dimlessLoad]
  
strDisFac :: QDefinition
strDisFac = mkQuantDef stressDistFac strDisFac_eq

strDisFacDD :: DataDefinition
strDisFacDD = mkDD strDisFac [{-references-}] [{-derivation-}] ""--temporary
  (Just $ jRef2 : qHtRef : arRef : [])

--DD5--

nonFL_eq :: Expr
nonFL_eq = ((sy tolLoad) * (sy mod_elas) * (sy min_thick) $^ 4) /
  (square (sy plate_len * sy plate_width))

nonFL :: QDefinition
nonFL = mkQuantDef nonFactorL nonFL_eq

nonFLDD :: DataDefinition
nonFLDD = mkDD nonFL [{-references-}] [{-derivation-}] ""--temporary
  (Just $ aGrtrThanB : hRef : qHtTlTolRef : [])

--DD6--

glaTyFac_eq :: Expr
glaTyFac_eq = (case_ (zipWith glaTyFac_helper glassTypeFactors glassTypeAbbrsStr))

glaTyFac_helper :: Integer -> String -> (Expr, Relation)
glaTyFac_helper result condition = (int result, (sy glass_type) $= str condition)

glaTyFac :: QDefinition
glaTyFac = mkQuantDef gTF glaTyFac_eq

glaTyFacDD :: DataDefinition
glaTyFacDD = mkDD glaTyFac [{-references-}] [{-derivation-}] ""--temporary
  (Just $ anGlass : ftGlass : hsGlass : [])

--DD7--

dimLL_eq :: Expr
dimLL_eq = ((sy demand) * (square (sy plate_len * sy plate_width)))
  / ((sy mod_elas) * (sy min_thick $^ 4) * (sy gTF))

dimLL :: QDefinition
dimLL = mkQuantDef dimlessLoad dimLL_eq

dimLLDD :: DataDefinition
dimLLDD = mkDD dimLL [sourceref $ S "[5, Eq. 7]"] [{-derivation-}] ""--temporary
  (Just $ qRef : aGrtrThanB : hRef : gtfRef : a5Ref : [])

--DD8--

tolPre_eq :: Expr
tolPre_eq = apply (sy tolLoad) [sy sdf_tol, (sy plate_len) / (sy plate_width)]
--tolPre_eq = FCall (asExpr interpY) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy sdf_tol]

tolPre :: QDefinition
tolPre = mkQuantDef tolLoad tolPre_eq

tolPreDD :: DataDefinition
tolPreDD = mkDD tolPre [{-references-}] [{-derivation-}] ""--temporary
  (Just $ qHtTlExtra : [])

--DD9--

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = ln (ln (1 / (1 - (sy pb_tol)))
  * ((((sy plate_len) * (sy plate_width)) $^ (sy sflawParamM - 1) / 
    ((sy sflawParamK) * ((sy mod_elas *
    (square (sy min_thick)))) $^ (sy sflawParamM) * (sy lDurFac)))))

tolStrDisFac :: QDefinition
tolStrDisFac = mkQuantDef sdf_tol tolStrDisFac_eq

tolStrDisFacDD :: DataDefinition
tolStrDisFacDD = mkDD tolStrDisFac [{-references-}] [{-derivation-}] ""--temporary
  (Just $ jtolRelToPbtol : aGrtrThanB : hRef : ldfRef : pbTolUsr : [])

--DD10--

standOffDis_eq :: Expr
standOffDis_eq = sqrt ((sy sdx) $^ 2 + (sy sdy) $^ 2 + (sy sdz) $^ 2)

standOffDis :: QDefinition
standOffDis = mkQuantDef standOffDist standOffDis_eq

standOffDisDD :: DataDefinition
standOffDisDD = mkDD standOffDis [{-references-}] [{-derivation-}] ""--temporary
  Nothing

--DD11--

aspRat_eq :: Expr
aspRat_eq = (sy plate_len) / (sy plate_width)

aspRat :: QDefinition
aspRat = mkQuantDef aspect_ratio aspRat_eq

aspRatDD :: DataDefinition
aspRatDD = mkDD aspRat [{-references-}] [{-derivation-}] ""--temporary
  (Just $ aGrtrThanB : [])

--Additional Notes--

aGrtrThanB :: Sentence
aGrtrThanB = (ch plate_len `sC` ch plate_width +:+ 
  S "are" +:+ plural dimension +:+ S "of the plate" `sC` S "where" +:+. 
  sParen (E (sy plate_len $>= sy plate_width)))

anGlass :: Sentence
anGlass = (getAcc annealed +:+ S "is" +:+ phrase annealed +:+ S "glass")

arRef :: Sentence
arRef = (ch aspect_ratio +:+ S "is the" +:+ phrase aspect_ratio +:+.
  S "defined in DD11")

ftGlass :: Sentence
ftGlass = (getAcc fullyT +:+ S "is" +:+ phrase fullyT +:+ S "glass")

hRef :: Sentence
hRef = (ch min_thick +:+ S "is the minimum thickness" `sC` 
  S "which is based on the nominal thicknesses" +:+. S "as shown in DD2")

hsGlass :: Sentence
hsGlass = (getAcc heatS +:+ S "is" +:+ phrase heatS +:+ S "glass")

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
  +:+ sParen (ch min_thick) +:+. S "to the minimum thickness")

qHtTlExtra :: Sentence
qHtTlExtra = (ch tolLoad +:+ S "is the tolerable load which is obtained from Figure 7 using" 
  +:+ ch sdf_tol `sAnd` phrase aspect_ratio +:+ S "as" +:+ plural parameter +:+. S "using interpolation" 
  +:+ titleize' calculation +:+ S "of" +:+ ch sdf_tol `sAnd` ch aspect_ratio +:+. 
  S "are defined in DD9 and DD11, respectively")

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

a5Ref :: Sentence 
a5Ref = (ch dimlessLoad +:+ S "is calculated with reference to" +:+. makeRef newA5)