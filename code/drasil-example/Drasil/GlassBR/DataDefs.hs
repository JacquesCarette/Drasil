module Drasil.GlassBR.DataDefs (aspRat, dataDefns, dimLL, gbQDefns, glaTyFac, 
  hFromt, loadDF, nonFL, risk, standOffDis, strDisFac, tolPre, tolStrDisFac) where

import Language.Drasil
import Prelude hiding (log, exp, sqrt)

import Data.Drasil.Concepts.Documentation (datum, user)
import Data.Drasil.Concepts.Math (probability, parameter, calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)

import Data.Drasil.Citations (campidelli)
import Data.Drasil.SentenceStructures (sAnd, sOf)

import Drasil.GlassBR.Concepts (annealed, fullyT, heatS)
import Drasil.GlassBR.Labels (calOfDemandL, glassLiteL)
import Drasil.GlassBR.References (astm2009, beasonEtAl1998)
import Drasil.GlassBR.Unitals (actualThicknesses, aspect_ratio, 
  demand, dimlessLoad, gTF, glassTypeAbbrsStr, glassTypeFactors, glass_type, 
  lDurFac, load_dur, mod_elas, nom_thick, nominalThicknesses, nonFactorL, pb_tol, 
  plate_len, plate_width, risk_fun, sdf_tol, sdx, sdy, sdz, standOffDist, sflawParamK, 
  sflawParamM, stressDistFac, tolLoad, min_thick)

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefns :: [DataDefinition] 
dataDefns = [risk, hFromt, loadDF, strDisFac, nonFL, glaTyFac, 
  dimLL, tolPre, tolStrDisFac, standOffDis, aspRat]

gbQDefns :: [Block QDefinition]
gbQDefns = [Parallel hFromtQD {-DD2-} [glaTyFacQD {-DD6-}]] ++ --can be calculated on their own
  map (\x -> Parallel x []) [dimLLQD {-DD7-}, strDisFacQD {-DD4-}, riskQD {-DD1-},
  tolStrDisFacQD {-DD9-}, tolPreQD {-DD8-}, nonFLQD {-DD5-}] 

--DD1--

risk_eq :: Expr
risk_eq = ((sy sflawParamK) / 
  ((sy plate_len) * (sy plate_width)) $^ ((sy sflawParamM) - 1) *
  (sy mod_elas * (square $ sy min_thick)) $^ (sy sflawParamM) 
  * (sy lDurFac) * (exp (sy stressDistFac)))

-- FIXME [4] !!!
riskQD :: QDefinition
riskQD = mkQuantDef risk_fun risk_eq

risk :: DataDefinition
risk = mkDD riskQD 
  [makeRef astm2009, makeRef beasonEtAl1998 +:+ sParen (S "Eq. 4-5"), 
  makeRef campidelli +:+ sParen (S "Eq. 14")] 
  [{-derivation-}] "risk_fun"
  (Just $ aGrtrThanB : hRef : ldfRef : jRef : [])

--DD2--

hFromt_eq :: Relation
hFromt_eq = (1/1000) * (case_ (zipWith hFromt_helper 
  actualThicknesses nominalThicknesses))

hFromt_helper :: Double -> Double -> (Expr, Relation)
hFromt_helper result condition = (dbl result, (sy nom_thick) $= dbl condition)

hFromtQD :: QDefinition
hFromtQD = mkQuantDef min_thick hFromt_eq

hFromt :: DataDefinition
hFromt = mkDD hFromtQD [{-references-}] [{-derivation-}] "min_thick"
  (Just $ [hMin])

--DD3-- (#749)

loadDF_eq :: Expr 
loadDF_eq = (sy load_dur / 60) $^ (sy sflawParamM / 16)

loadDFQD :: QDefinition
loadDFQD = mkQuantDef lDurFac loadDF_eq

loadDF :: DataDefinition
loadDF = mkDD loadDFQD [{-references-}] [{-derivation-}] "loadDurFactor"
  Nothing

--DD4--

strDisFac_eq :: Expr
strDisFac_eq = apply (sy stressDistFac) 
  [sy dimlessLoad, sy aspect_ratio]
--strDisFac_eq = FCall (asExpr interpZ) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy dimlessLoad]
  
strDisFacQD :: QDefinition
strDisFacQD = mkQuantDef stressDistFac strDisFac_eq

strDisFac :: DataDefinition
strDisFac = mkDD strDisFacQD [{-references-}] [{-derivation-}] "stressDistFac"
  (Just $ jRef2 : qHtRef : arRef : [])

--DD5--

nonFL_eq :: Expr
nonFL_eq = ((sy tolLoad) * (sy mod_elas) * (sy min_thick) $^ 4) /
  (square (sy plate_len * sy plate_width))

nonFLQD :: QDefinition
nonFLQD = mkQuantDef nonFactorL nonFL_eq

nonFL :: DataDefinition
nonFL = mkDD nonFLQD [{-references-}] [{-derivation-}] "nFL"
  (Just $ aGrtrThanB : hRef : qHtTlTolRef : [])

--DD6--

glaTyFac_eq :: Expr
glaTyFac_eq = (case_ (zipWith glaTyFac_helper glassTypeFactors glassTypeAbbrsStr))

glaTyFac_helper :: Integer -> String -> (Expr, Relation)
glaTyFac_helper result condition = (int result, (sy glass_type) $= str condition)

glaTyFacQD :: QDefinition
glaTyFacQD = mkQuantDef gTF glaTyFac_eq

glaTyFac :: DataDefinition
glaTyFac = mkDD glaTyFacQD [{-references-}] [{-derivation-}] "gTF"
  (Just $ anGlass : ftGlass : hsGlass : [])

--DD7--

dimLL_eq :: Expr
dimLL_eq = ((sy demand) * (square (sy plate_len * sy plate_width)))
  / ((sy mod_elas) * (sy min_thick $^ 4) * (sy gTF))

dimLLQD :: QDefinition
dimLLQD = mkQuantDef dimlessLoad dimLL_eq

dimLL :: DataDefinition
dimLL = mkDD dimLLQD [makeRef campidelli +:+ sParen (S "Eq. 7")] [{-derivation-}] "dimlessLoad"
  (Just $ qRef : aGrtrThanB : hRef : gtfRef : glassLiteRef : [])

--DD8--

tolPre_eq :: Expr
tolPre_eq = apply (sy tolLoad) [sy sdf_tol, (sy plate_len) / (sy plate_width)]
--tolPre_eq = FCall (asExpr interpY) [V "SDF.txt", (sy plate_len) / (sy plate_width), sy sdf_tol]

tolPreQD :: QDefinition
tolPreQD = mkQuantDef tolLoad tolPre_eq

tolPre :: DataDefinition
tolPre = mkDD tolPreQD [{-references-}] [{-derivation-}] "tolLoad"
  (Just $ qHtTlExtra : [])

--DD9--

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = ln (ln (1 / (1 - (sy pb_tol)))
  * ((((sy plate_len) * (sy plate_width)) $^ (sy sflawParamM - 1) / 
    ((sy sflawParamK) * ((sy mod_elas *
    (square (sy min_thick)))) $^ (sy sflawParamM) * (sy lDurFac)))))

tolStrDisFacQD :: QDefinition
tolStrDisFacQD = mkQuantDef sdf_tol tolStrDisFac_eq

tolStrDisFac :: DataDefinition
tolStrDisFac = mkDD tolStrDisFacQD [{-references-}] [{-derivation-}] "sdf_tol"
  (Just $ jtolRelToPbtol : aGrtrThanB : hRef : ldfRef : pbTolUsr : [])

--DD10--

standOffDis_eq :: Expr
standOffDis_eq = sqrt ((sy sdx) $^ 2 + (sy sdy) $^ 2 + (sy sdz) $^ 2)

standOffDisQD :: QDefinition
standOffDisQD = mkQuantDef standOffDist standOffDis_eq

standOffDis :: DataDefinition
standOffDis = mkDD standOffDisQD [{-references-}] [{-derivation-}] "standOffDist"
  Nothing

--DD11--

aspRat_eq :: Expr
aspRat_eq = (sy plate_len) / (sy plate_width)

aspRatQD :: QDefinition
aspRatQD = mkQuantDef aspect_ratio aspRat_eq

aspRat :: DataDefinition
aspRat = mkDD aspRatQD [{-references-}] [{-derivation-}] "aspect_ratio"
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
  S "defined in" +:+ makeRef aspRat)

ftGlass :: Sentence
ftGlass = (getAcc fullyT +:+ S "is" +:+ phrase fullyT +:+ S "glass")

hRef :: Sentence
hRef = (ch min_thick +:+ S "is the" +:+ phrase min_thick `sC` 
  S "which is based on the nominal thicknesses as shown in" +:+. makeRef hFromt)

hsGlass :: Sentence
hsGlass = (getAcc heatS +:+ S "is" +:+ phrase heatS +:+ S "glass")

ldfRef :: Sentence
ldfRef = (ch lDurFac +:+ S "is the" +:+ phrase lDurFac +:+ S "as defined by" +:+. makeRef loadDF)

pbTolUsr :: Sentence
pbTolUsr = (ch pb_tol +:+ S "is the tolerable" +:+ phrase probability +:+ S "entered by the" +:+. 
  phrase user)

jRef :: Sentence
jRef = (ch stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` S "as defined in" +:+. 
  makeRef strDisFac)

hMin :: Sentence
hMin = (ch nom_thick +:+ S "is a function that maps from the nominal thickness"
  +:+ sParen (ch min_thick) +:+ S "to the" +:+. phrase min_thick)

qHtTlExtra :: Sentence
qHtTlExtra = (ch tolLoad +:+ S "is the tolerable load which is obtained from Figure 7 using" 
  +:+ ch sdf_tol `sAnd` phrase aspect_ratio +:+ S "as" +:+ plural parameter +:+. S "using interpolation" 
  +:+ titleize' calculation `sOf` ch sdf_tol `sAnd` ch aspect_ratio +:+ 
  S "are defined in" +:+. makeRef tolStrDisFac `sAnd` makeRef aspRat `sC` S "respectively")

qHtTlTolRef :: Sentence
qHtTlTolRef = (ch tolLoad +:+ S "is the tolerable load defined in" +:+. makeRef tolPre)

qRef :: Sentence
qRef = (ch demand +:+ S "is the 3 second equivalent pressure, as given in" +:+. makeRef calOfDemandL)

gtfRef :: Sentence
gtfRef = (ch gTF +:+ S "is the" +:+. (phrase gTF `sC` S "as given by" +:+ makeRef glaTyFac))

qHtRef :: Sentence
qHtRef = (ch dimlessLoad +:+ S "is the" +:+ phrase dimlessLoad +:+ S "defined in" +:+. makeRef dimLL)

jRef2 :: Sentence
jRef2 = (ch stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` 
  S "which is obtained by" +:+ S "interpolating from" +:+ plural datum +:+. 
  S "shown in Figure 7")

jtolRelToPbtol :: Sentence
jtolRelToPbtol = (ch sdf_tol +:+ S " is calculated with reference to " +:+. ch pb_tol)

glassLiteRef :: Sentence 
glassLiteRef = (ch dimlessLoad +:+ S "is calculated with reference to" +:+. makeRef glassLiteL)