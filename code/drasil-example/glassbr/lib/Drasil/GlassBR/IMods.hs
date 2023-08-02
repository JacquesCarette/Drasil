module Drasil.GlassBR.IMods {- temporarily export everything-}
-- (symb, iMods, pbIsSafe, lrIsSafe, instModIntro)
  where

import Control.Lens ((^.))
import Prelude hiding (exp)
import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDeriv, qwC, qwUC, equationalModelN)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations (campidelli)
import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.SI_Units

import Drasil.GlassBR.DataDefs {- temporarily import everything -}
import Drasil.GlassBR.Figures (dimlessloadVsARFig)
import Drasil.GlassBR.Goals (willBreakGS)
import Drasil.GlassBR.References (astm2009, beasonEtAl1998)
import Drasil.GlassBR.Unitals {- temporarily import everything -}
import Drasil.SRSDocument (Block (Parallel))

iMods :: [InstanceModel]
iMods = [risk, strDisFac, nonFL, dimLL, tolPre, probOfBreak, calofCapacity,
  pbIsSafe, lrIsSafe]

symb :: [UnitalChunk]
symb =  [ucuc plateLen metre, ucuc plateWidth metre, ucuc charWeight kilogram, ucuc standOffDist metre, demand] -- this is temporary
-- ++
 -- [dqdQd (qw calofDemand) demandq]

qDefns :: [Block SimpleQDef]
qDefns = Parallel hFromtQD [glaTyFacQD] : --can be calculated on their own
  map (`Parallel` []) [dimLLQD, strDisFacQD, riskQD, tolStrDisFacQD, tolPreQD,
    nonFLQD]

abInputConstraints :: [(QuantityDict, Maybe (RealInterval Expr Expr))]
abInputConstraints = [qwC plateLen   $ UpFrom  (Exc, exactDbl 0),
                      qwC plateWidth $ Bounded (Exc, exactDbl 0) (Inc, sy plateLen)]

aspectRatioConstraint :: (QuantityDict, Maybe (RealInterval Expr Expr))
aspectRatioConstraint = qwC aspectRatio (UpFrom (Inc, exactDbl 1))

{--}

risk :: InstanceModel
risk = imNoDeriv (equationalModelN (riskFun ^. term) riskQD)
  (qwUC modElas : qwUC lDurFac : qwUC stressDistFac :
    map qwUC [sflawParamK, sflawParamM, minThick] ++ abInputConstraints)
  (qw riskFun) [] [dRef astm2009, dRefInfo beasonEtAl1998 $ Equation [4, 5],
    dRefInfo campidelli $ Equation [14]] "riskFun" [aGrtrThanB, hRef, ldfRef, jRef]

-- FIXME [4] !!!
riskQD :: SimpleQDef
riskQD = mkQuantDef riskFun ((sy sflawParamK $/
  (mulRe (sy plateLen) (sy plateWidth) $^ (sy sflawParamM $- exactDbl 1))) `mulRe`
  ((sy modElas `mulRe` square (sy minThick)) $^ sy sflawParamM) `mulRe` sy lDurFac `mulRe` exp (sy stressDistFac))

{--}

strDisFac :: InstanceModel
strDisFac = imNoDeriv (equationalModelN (stressDistFac ^. term) strDisFacQD)
  (aspectRatioConstraint : [qwUC dimlessLoad]) (qw stressDistFac)
  [Bounded (Inc, sy stressDistFacMin) (Inc, sy stressDistFacMax)]
  [dRef astm2009] "stressDistFac"
  [interpolating stressDistFac dimlessloadVsARFig, arRef, qHtRef]

strDisFacQD :: SimpleQDef
strDisFacQD = mkQuantDef stressDistFac strDisFacEq

strDisFacEq :: Expr
-- strDisFacEq = apply (sy stressDistFac)
--   [sy dimlessLoad, sy aspectRatio]
strDisFacEq = apply interpZ [str "SDF.txt", sy aspectRatio, sy dimlessLoad]

{--}

nonFL :: InstanceModel
nonFL = imNoDeriv (equationalModelN (nonFactorL ^. term) nonFLQD)
  (qwUC tolLoad : qwUC modElas : qwUC minThick : abInputConstraints)
  (qw nonFactorL) [] [dRef astm2009] "nFL"
  [qHtTlTolRef, stdVals [modElas], hRef, aGrtrThanB]

nonFLEq :: Expr
nonFLEq = mulRe (mulRe (sy tolLoad) (sy modElas)) (sy minThick $^ exactDbl 4) $/
  square (mulRe (sy plateLen) (sy plateWidth))

nonFLQD :: SimpleQDef
nonFLQD = mkQuantDef nonFactorL nonFLEq

{--}

dimLL :: InstanceModel
dimLL = imNoDeriv (equationalModelN (dimlessLoad ^. term) dimLLQD)
  (qwUC demand : qwUC modElas : qwUC minThick : qwUC gTF : abInputConstraints)
  (qw dimlessLoad) [] [dRef astm2009, dRefInfo campidelli $ Equation [7]]
  "dimlessLoad" [qRef, aGrtrThanB, stdVals [modElas], hRef, gtfRef]

dimLLEq :: Expr
dimLLEq = mulRe (sy demand) (square (mulRe (sy plateLen) (sy plateWidth)))
  $/ mulRe (mulRe (sy modElas) (sy minThick $^ exactDbl 4)) (sy gTF)

dimLLQD :: SimpleQDef
dimLLQD = mkQuantDef dimlessLoad dimLLEq

{--}

tolPre :: InstanceModel
tolPre = imNoDeriv (equationalModelN (tolLoad ^. term) tolPreQD)
  [aspectRatioConstraint, qwUC tolStrDisFac] (qw tolLoad) [] [dRef astm2009]
  "tolLoad" [interpolating tolLoad dimlessloadVsARFig, arRef, jtolRef]

tolPreEq :: Expr
--tolPreEq = apply (sy tolLoad) [sy sdfTol, (sy plateLen) / (sy plateWidth)]
tolPreEq = apply interpY [str "SDF.txt", sy aspectRatio, sy sdfTol]

tolPreQD :: SimpleQDef
tolPreQD = mkQuantDef tolLoad tolPreEq

{--}

probOfBreak :: InstanceModel
probOfBreak = imNoDeriv (equationalModelN (probBr ^. term) probOfBreakQD)
  [qwUC risk] (qw probBr) [] (map dRef [astm2009, beasonEtAl1998]) "probOfBreak"
  [riskRef]

probOfBreakQD :: SimpleQDef
probOfBreakQD = mkQuantDef probBr (exactDbl 1 $- exp (neg (sy risk)))

{--}

calofCapacity :: InstanceModel
calofCapacity = imNoDeriv (equationalModelN (lRe ^. term) calofCapacityQD)
  (qwUC nonFL : qwUC glaTyFac : [qwUC loadSF]) (qw lRe) []
  [dRef astm2009] "calofCapacity" [lrCap, nonFLRef, gtfRef]

calofCapacityQD :: SimpleQDef
calofCapacityQD = mkQuantDef lRe (sy nonFL `mulRe` sy glaTyFac `mulRe` sy loadSF)

{--}

pbIsSafe :: InstanceModel
pbIsSafe = imNoDeriv (equationalModelN (nounPhraseSP "Safety Req-Pb") pbIsSafeQD)
  [qwC probBr $ UpFrom (Exc, exactDbl 0), qwC pbTol $ UpFrom (Exc, exactDbl 0)]
  (qw isSafePb) []
  [dRef astm2009] "isSafePb"
  [pbIsSafeDesc, probBRRef, pbTolUsr]

pbIsSafeQD :: SimpleQDef
pbIsSafeQD = mkQuantDef isSafePb (sy probBr $< sy pbTol)

{--}

lrIsSafe :: InstanceModel
lrIsSafe = imNoDeriv (equationalModelN (nounPhraseSP "Safety Req-LR") lrIsSafeQD)
  [qwC lRe $ UpFrom (Exc, exactDbl 0), qwC demand $ UpFrom (Exc, exactDbl 0)]
  (qw isSafeLR) []
  [dRef astm2009] "isSafeLR"
  [lrIsSafeDesc, capRef, qRef]

lrIsSafeQD :: SimpleQDef 
lrIsSafeQD = mkQuantDef isSafeLR (sy lRe $> sy demand)

iModDesc :: QuantityDict -> Sentence -> Sentence
iModDesc main s = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
    S "considered safe", s `S.are` S "either both True or both False"]
  
-- Intro --

instModIntro :: Sentence
instModIntro = foldlSent [atStartNP (the goal), refS willBreakGS, 
  S "is met by", refS pbIsSafe `sC` refS lrIsSafe]

-- Notes --

capRef :: Sentence
capRef = definedIn' calofCapacity (S "and is also called capacity")

lrIsSafeDesc :: Sentence
lrIsSafeDesc = iModDesc isSafeLR
  (ch isSafePb +:+ fromSource pbIsSafe `S.and_` ch isSafeLR)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = iModDesc isSafePb
  (ch isSafePb `S.and_` ch isSafePb +:+ fromSource lrIsSafe)

jRef, nonFLRef, probBRRef, qHtRef, qHtTlTolRef, riskRef :: Sentence
jRef        = definedIn strDisFac
nonFLRef    = definedIn nonFL
probBRRef   = definedIn probOfBreak
qHtRef      = definedIn dimLL
qHtTlTolRef = definedIn tolPre
riskRef     = definedIn risk
