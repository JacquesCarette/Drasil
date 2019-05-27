module Drasil.GlassBR.DataDefs (aspRat, dataDefns, dimLL, qDefns, glaTyFac, 
  hFromt, loadDF, nonFL, risk, standOffDis, strDisFac, tolPre, tolStrDisFac, 
  eqTNTWDD, probOfBreak, calofCapacity, calofDemand) where
import Control.Lens ((^.))
import Language.Drasil
import Language.Drasil.Code (asExpr')
import Prelude hiding (log, exp, sqrt)
import Theory.Drasil (DataDefinition, dd, mkQuantDef)
import Database.Drasil (Block(Parallel))

import Data.Drasil.Concepts.Documentation (datum, user)
import Data.Drasil.Concepts.Math (probability, parameter, calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)

import Data.Drasil.Citations (campidelli)
import Data.Drasil.SentenceStructures (sAnd, sOf, foldlSent, isThe, sOr)

import Drasil.GlassBR.Assumptions (assumpSV, assumpLDFC, assumpGL)
import Drasil.GlassBR.Concepts (annealed, fullyT, heatS)
import Drasil.GlassBR.ModuleDefs (interpY, interpZ)
import Drasil.GlassBR.References (astm2009, beasonEtAl1998)
import Drasil.GlassBR.Unitals (actualThicknesses, aspect_ratio, charWeight,
  demand, dimlessLoad, gTF, glassType, glassTypeFactors, glass_type, 
  lDurFac, loadDur, modElas, nomThick, nominalThicknesses, nonFactorL, pbTol, 
  plateLen, plateWidth, riskFun, sdfTol, sdx, sdy, sdz, standOffDist, sflawParamK, 
  sflawParamM, stressDistFac, tNT, tolLoad, minThick, probBr, lRe, loadSF,
  demandq, eqTNTWeight)

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefns :: [DataDefinition] 
dataDefns = [risk, hFromt, loadDF, strDisFac, nonFL, glaTyFac, 
  dimLL, tolPre, tolStrDisFac, standOffDis, aspRat, eqTNTWDD, probOfBreak,
  calofCapacity, calofDemand]

qDefns :: [Block QDefinition]
qDefns = Parallel hFromtQD {-DD2-} [glaTyFacQD {-DD6-}] : --can be calculated on their own
  map (flip Parallel []) [dimLLQD {-DD7-}, strDisFacQD {-DD4-}, riskQD {-DD1-},
  tolStrDisFacQD {-DD9-}, tolPreQD {-DD8-}, nonFLQD {-DD5-}] 

--DD1--

riskEq :: Expr
riskEq = ((sy sflawParamK) / 
  ((sy plateLen) * (sy plateWidth)) $^ ((sy sflawParamM) - 1) *
  (sy modElas * square (sy minThick)) $^ (sy sflawParamM) 
  * (sy lDurFac) * (exp (sy stressDistFac)))

-- FIXME [4] !!!
riskQD :: QDefinition
riskQD = mkQuantDef riskFun riskEq

risk :: DataDefinition
risk = dd riskQD 
  (map makeCite [astm2009, beasonEtAl1998 {- FIXME +:+ sParen (S "Eq. 4-5") -},
  campidelli {- FIXME +:+ sParen (S "Eq. 14") -}])
  [{-derivation-}] "riskFun"
  [aGrtrThanB, hRef, ldfRef, jRef]

--DD2--

hFromtEq :: Relation
hFromtEq = (1/1000) * (case_ (zipWith hFromtHelper 
  actualThicknesses nominalThicknesses))

hFromtHelper :: Double -> Double -> (Expr, Relation)
hFromtHelper result condition = (dbl result, (sy nomThick) $= dbl condition)

hFromtQD :: QDefinition
hFromtQD = mkQuantDef minThick hFromtEq

hFromt :: DataDefinition
hFromt = dd hFromtQD [makeCite astm2009] [{-derivation-}] "minThick" [hMin]

--DD3-- (#749)

loadDFEq :: Expr 
loadDFEq = (sy loadDur / 60) $^ (sy sflawParamM / 16)

loadDFQD :: QDefinition
loadDFQD = mkQuantDef lDurFac loadDFEq

loadDF :: DataDefinition
loadDF = dd loadDFQD [makeCite astm2009] [{-derivation-}] "loadDurFactor" [makeRef2S assumpSV,
  makeRef2S assumpLDFC]

--DD4--

strDisFacEq :: Expr
-- strDisFacEq = apply (sy stressDistFac)
--   [sy dimlessLoad, sy aspect_ratio]
strDisFacEq = apply (asExpr' interpZ) [Str "SDF.txt", sy aspect_ratio, sy dimlessLoad]
  
strDisFacQD :: QDefinition
strDisFacQD = mkQuantDef stressDistFac strDisFacEq

strDisFac :: DataDefinition
strDisFac = dd strDisFacQD [makeCite astm2009] [{-derivation-}] "stressDistFac"
  [jRef2, qHtRef, arRef]

--DD5--

nonFLEq :: Expr
nonFLEq = ((sy tolLoad) * (sy modElas) * (sy minThick) $^ 4) /
  (square (sy plateLen * sy plateWidth))

nonFLQD :: QDefinition
nonFLQD = mkQuantDef nonFactorL nonFLEq

nonFL :: DataDefinition
nonFL = dd nonFLQD [makeCite astm2009] [{-derivation-}] "nFL"
  (aGrtrThanB : hRef : qHtTlTolRef : [makeRef2S assumpSV])

--DD6--

glaTyFacEq :: Expr
glaTyFacEq = (case_ (zipWith glaTyFacHelper glassTypeFactors $ map (getAccStr . snd) glassType))

glaTyFacHelper :: Integer -> String -> (Expr, Relation)
glaTyFacHelper result condition = (int result, (sy glass_type) $= str condition)

glaTyFacQD :: QDefinition
glaTyFacQD = mkQuantDef gTF glaTyFacEq

glaTyFac :: DataDefinition
glaTyFac = dd glaTyFacQD [makeCite astm2009] [{-derivation-}] "gTF"
  [anGlass, ftGlass, hsGlass]

--DD7--

dimLLEq :: Expr
dimLLEq = ((sy demand) * (square (sy plateLen * sy plateWidth)))
  / ((sy modElas) * (sy minThick $^ 4) * (sy gTF))

dimLLQD :: QDefinition
dimLLQD = mkQuantDef dimlessLoad dimLLEq

dimLL :: DataDefinition
dimLL = dd dimLLQD (map makeCite [astm2009, campidelli {- +:+ sParen (S "Eq. 7") -}]) [{-derivation-}] "dimlessLoad"
  [qRef , aGrtrThanB , hRef, gtfRef, glassLiteRef, makeRef2S assumpSV]

--DD8--

tolPreEq :: Expr
--tolPreEq = apply (sy tolLoad) [sy sdfTol, (sy plateLen) / (sy plateWidth)]
tolPreEq = apply (asExpr' interpY) [Str "SDF.txt", sy aspect_ratio, sy sdfTol]

tolPreQD :: QDefinition
tolPreQD = mkQuantDef tolLoad tolPreEq

tolPre :: DataDefinition
tolPre = dd tolPreQD [makeCite astm2009] [{-derivation-}] "tolLoad"
  [qHtTlExtra]

--DD9--

tolStrDisFacEq :: Expr
tolStrDisFacEq = ln (ln (1 / (1 - (sy pbTol)))
  * ((((sy plateLen) * (sy plateWidth)) $^ (sy sflawParamM - 1) / 
    ((sy sflawParamK) * ((sy modElas *
    (square (sy minThick)))) $^ (sy sflawParamM) * (sy lDurFac)))))

tolStrDisFacQD :: QDefinition
tolStrDisFacQD = mkQuantDef sdfTol tolStrDisFacEq

tolStrDisFac :: DataDefinition
tolStrDisFac = dd tolStrDisFacQD [makeCite astm2009] [{-derivation-}] "sdfTol"
  (jtolRelToPbtol : aGrtrThanB : hRef : ldfRef : pbTolUsr : [makeRef2S assumpSV])

--DD10--

standOffDisEq :: Expr
standOffDisEq = sqrt ((sy sdx) $^ 2 + (sy sdy) $^ 2 + (sy sdz) $^ 2)

standOffDisQD :: QDefinition
standOffDisQD = mkQuantDef standOffDist standOffDisEq

standOffDis :: DataDefinition
standOffDis = dd standOffDisQD [makeCite astm2009] [{-derivation-}] "standOffDist" []

--DD11--

aspRatEq :: Expr
aspRatEq = (sy plateLen) / (sy plateWidth)

aspRatQD :: QDefinition
aspRatQD = mkQuantDef aspect_ratio aspRatEq

aspRat :: DataDefinition
aspRat = dd aspRatQD [makeCite astm2009] [{-derivation-}] "aspect_ratio" [aGrtrThanB]

--DD12--
eqTNTWEq :: Expr
eqTNTWEq = (sy charWeight) * (sy tNT)

eqTNTWQD :: QDefinition
eqTNTWQD = mkQuantDef eqTNTWeight eqTNTWEq

eqTNTWDD :: DataDefinition
eqTNTWDD = dd eqTNTWQD [makeCite astm2009] [] "eqTNTW" []

--DD13--
probOfBreakEq :: Expr
probOfBreakEq = 1 - (exp (negate (sy risk)))

probOfBreakQD :: QDefinition
probOfBreakQD = mkQuantDef probBr probOfBreakEq

probOfBreak :: DataDefinition
probOfBreak = dd probOfBreakQD (map makeCite [astm2009, beasonEtAl1998]) [{-derivation-}] "probOfBreak" [glassBreak]

--DD14--
calofCapacityEq :: Expr
calofCapacityEq = ((sy nonFL) * (sy glaTyFac) * (sy loadSF))

calofCapacityQD :: QDefinition
calofCapacityQD = mkQuantDef lRe calofCapacityEq

calofCapacity :: DataDefinition
calofCapacity = dd calofCapacityQD [makeCite astm2009] [{-derivation-}] "calofCapacity" capacityS

--DD15--
calofDemandEq :: Expr
calofDemandEq = apply (asExpr' interpY) [Str "TSD.txt", sy standOffDist, sy eqTNTWeight]

calofDemandQD :: QDefinition
calofDemandQD = mkQuantDef demand calofDemandEq

calofDemand :: DataDefinition
calofDemand = dd calofDemandQD [makeCite astm2009] [{-derivation-}] "calofDemand" [calofDemandDesc]


--Additional Notes--
calofDemandDesc :: Sentence
calofDemandDesc = 
  foldlSent [(ch demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (ch standOffDist) `sAnd`
  (ch eqTNTWeight), S "as" +:+. plural parameter, 
  (ch eqTNTWeight), S "is defined in" +:+.
  makeRef2S eqTNTWDD, (ch standOffDist) `isThe`
  (phrase standOffDist), S "as defined in", makeRef2S standOffDis]

capacityS :: [Sentence]
capacityS = [ch lRe +:+ S "is the" +:+ phrase lRe `sC` S "which is also called capacity" +:+.
  ch nonFL +:+ S "is the" +:+ phrase nonFL `sC` S "as defined in" +:+.
  makeRef2S nonFL +:+ ch glaTyFac +:+ S "is the" +:+ phrase glaTyFac `sC` S "as defined in" +:+.
  makeRef2S glaTyFac, makeRef2S assumpGL, makeRef2S glaTyFac, makeRef2S nonFL]


glassBreak :: Sentence
glassBreak = (ch risk +:+ S "is the" +:+ phrase risk `sC` S "as defined in" +:+
  makeRef2S risk)

aGrtrThanB :: Sentence
aGrtrThanB = (ch plateLen `sC` ch plateWidth +:+ 
  S "are" +:+ plural dimension +:+ S "of the plate" `sC` S "where" +:+. 
  sParen (E (sy plateLen $>= sy plateWidth)))

anGlass :: Sentence
anGlass = (getAcc annealed +:+ S "is" +:+ phrase annealed +:+ S "glass")

arRef :: Sentence
arRef = (ch aspect_ratio +:+ S "is the" +:+ phrase aspect_ratio +:+.
  S "defined in" +:+ makeRef2S aspRat)

ftGlass :: Sentence
ftGlass = (getAcc fullyT +:+ S "is" +:+ phrase fullyT +:+ S "glass")

hRef :: Sentence
hRef = (ch minThick +:+ S "is the" +:+ phrase minThick `sC` 
  S "which is based on the nominal thicknesses as shown in" +:+. makeRef2S hFromt)

hsGlass :: Sentence
hsGlass = (getAcc heatS +:+ S "is" +:+ phrase heatS +:+ S "glass")

ldfRef :: Sentence
ldfRef = (ch lDurFac +:+ S "is the" +:+ phrase lDurFac +:+ S "as defined by" +:+. makeRef2S loadDF)

pbTolUsr :: Sentence
pbTolUsr = (ch pbTol +:+ S "is the tolerable" +:+ phrase probability +:+ S "entered by the" +:+. 
  phrase user)

jRef :: Sentence
jRef = (ch stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` S "as defined in" +:+. 
  makeRef2S strDisFac)

hMin :: Sentence
hMin = (ch nomThick +:+ S "is a function that maps from the nominal thickness"
  +:+ sParen (ch minThick) +:+ S "to the" +:+. phrase minThick)

qHtTlExtra :: Sentence
qHtTlExtra = (ch tolLoad +:+ S "is the tolerable load which is obtained from Figure 7 using" 
  +:+ ch sdfTol `sAnd` phrase aspect_ratio +:+ S "as" +:+ plural parameter +:+. S "using interpolation" 
  +:+ titleize' calculation `sOf` ch sdfTol `sAnd` ch aspect_ratio +:+ 
  S "are defined in" +:+. makeRef2S tolStrDisFac `sAnd` makeRef2S aspRat `sC` S "respectively")

qHtTlTolRef :: Sentence
qHtTlTolRef = (ch tolLoad +:+ S "is the tolerable load defined in" +:+. makeRef2S tolPre)

qRef :: Sentence
qRef = (ch demand +:+ S "is the 3 second equivalent pressure, as given in" +:+. makeRef2S calofDemand)

gtfRef :: Sentence
gtfRef = (ch gTF +:+ S "is the" +:+. (phrase gTF `sC` S "as given by" +:+ makeRef2S glaTyFac))

qHtRef :: Sentence
qHtRef = (ch dimlessLoad +:+ S "is the" +:+ phrase dimlessLoad +:+ S "defined in" +:+. makeRef2S dimLL)

jRef2 :: Sentence
jRef2 = (ch stressDistFac +:+ S "is the" +:+ phrase stressDistFac `sC` 
  S "which is obtained by" +:+ S "interpolating from" +:+ plural datum +:+. 
  S "shown in Figure 7")

jtolRelToPbtol :: Sentence
jtolRelToPbtol = (ch sdfTol +:+ S " is calculated with reference to " +:+. ch pbTol)

glassLiteRef :: Sentence 
glassLiteRef = (ch dimlessLoad +:+ S "is calculated with reference to" +:+. makeRef2S assumpGL)
