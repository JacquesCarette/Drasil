module Drasil.GlassBR.IMods (symb, iMods, pbIsSafe, lrIsSafe, instModIntro) where

import Control.Lens ((^.))
import Prelude hiding (exp)

import Drasil.Sentence.Combinators (definedIn', definedIn)
import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (InstanceModel, imNoDeriv, qwC, qwUC, equationalModelN, output)

import Data.Drasil.Citations (campidelli)
import Data.Drasil.Concepts.Documentation (goal, user)
import Data.Drasil.SI_Units (kilogram, metre)

import Drasil.GlassBR.DataDefs (aGrtrThanB, glaTyFac,
  gtfRef, hRef, loadDFDD, stdVals)
import Drasil.GlassBR.Goals (willBreakGS)
import Drasil.GlassBR.References (astm2009, beasonEtAl1998)
import Drasil.GlassBR.Unitals

iMods :: [InstanceModel]
iMods = [risk, nonFL, dimLL, tolStrDisFac, probOfBreak, calofCapacity,
  pbIsSafe, lrIsSafe]

symb :: [UnitalChunk]
symb = [ucuc plateLen metre, ucuc plateWidth metre, ucuc charWeight kilogram,
  ucuc standOffDist metre] -- this is temporary

abInputConstraints :: [(DefinedQuantityDict, Maybe (RealInterval Expr Expr))]
abInputConstraints = [qwC plateLen   $ UpFrom  (Exc, exactDbl 0),
                      qwC plateWidth $ Bounded (Exc, exactDbl 0) (Inc, sy plateLen)]

probConstraint :: RealInterval Expr Expr
probConstraint = Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)

{--}

risk :: InstanceModel
risk = imNoDeriv (equationalModelN (riskFun ^. term) riskQD)
  (qwUC modElas : qwUC loadDF : qwUC stressDistFac :
    map qwUC [sflawParamK, sflawParamM, minThick] ++ abInputConstraints)
    riskFun [] [dRef astm2009, dRefInfo beasonEtAl1998 $ Equation [4, 5],
    dRefInfo campidelli $ Equation [14]] "riskFun" [aGrtrThanB, hRef, ldfRef]

-- FIXME [4] !!!
riskQD :: SimpleQDef
riskQD = mkQuantDef riskFun ((sy sflawParamK $/
  (sy plateLen $* sy plateWidth $^ (sy sflawParamM $- exactDbl 1))) $*
  ((sy modElas $* square (sy minThick)) $^ sy sflawParamM) $* sy loadDF $* exp (sy stressDistFac))

{--}

nonFL :: InstanceModel
nonFL = imNoDeriv (equationalModelN (nonFactorL ^. term) nonFLQD)
  (qwUC tolLoad : qwUC modElas : qwUC minThick : abInputConstraints)
  (dqdWr nonFactorL) [] [dRef astm2009] "nFL"
  [stdVals [modElas], hRef, aGrtrThanB]

nonFLEq :: Expr
nonFLEq = (sy tolLoad $* sy modElas) $* (sy minThick $^ exactDbl 4) $/
  square (sy plateLen $* sy plateWidth)

nonFLQD :: SimpleQDef
nonFLQD = mkQuantDef nonFactorL nonFLEq

{--}

dimLL :: InstanceModel
dimLL = imNoDeriv (equationalModelN (dimlessLoad ^. term) dimLLQD)
  (qwUC demand : qwUC modElas : qwUC minThick : qwUC gTF : abInputConstraints)
  dimlessLoad [] [dRef astm2009, dRefInfo campidelli $ Equation [7]]
  "dimlessLoad" [aGrtrThanB, stdVals [modElas], hRef, gtfRef]

dimLLEq :: Expr
dimLLEq = sy demand $* square (sy plateLen $* sy plateWidth)
  $/ (sy modElas $* (sy minThick $^ exactDbl 4)) $* sy gTF

dimLLQD :: SimpleQDef
dimLLQD = mkQuantDef dimlessLoad dimLLEq

{--}

tolStrDisFac :: InstanceModel
tolStrDisFac = imNoDeriv (equationalModelN (sdfTol ^. term) tolStrDisFacQD)
  ((loadDF, Nothing) : qwC pbTol probConstraint : qwUC modElas : abInputConstraints ++
    map qwUC [sflawParamM, sflawParamK, minThick]) sdfTol []
  [dRef astm2009] "sdfTol" [pbTolUsr, aGrtrThanB, stdVals [sflawParamM,
      sflawParamK, modElas], hRef, ldfRef]

tolStrDisFacQD :: SimpleQDef
tolStrDisFacQD = mkQuantDef sdfTol $ ln (ln (recip_ (exactDbl 1 $- sy pbTol))
  $* ((sy plateLen $* sy plateWidth) $^ (sy sflawParamM $- exactDbl 1) $/
    (sy sflawParamK $* ((sy modElas $*
    square (sy minThick)) $^ sy sflawParamM) $* sy loadDF)))

{--}

probOfBreak :: InstanceModel
probOfBreak = imNoDeriv (equationalModelN (probBr ^. term) probOfBreakQD)
  [qwUC $ risk ^. output] (dqdWr probBr) [probConstraint] (map dRef [astm2009, beasonEtAl1998]) "probOfBreak"
  [riskRef]

probOfBreakQD :: SimpleQDef
probOfBreakQD = mkQuantDef probBr (exactDbl 1 $- exp (neg $ sy $ risk ^. output))

{--}

calofCapacity :: InstanceModel
calofCapacity = imNoDeriv (equationalModelN (lRe ^. term) calofCapacityQD)
  (qwUC (nonFL ^. output) : qwUC (glaTyFac ^. output) : [qwUC loadSF]) (dqdWr lRe) []
  [dRef astm2009] "calofCapacity" [lrCap, nonFLRef, gtfRef]

calofCapacityQD :: SimpleQDef
calofCapacityQD = mkQuantDef lRe (sy (nonFL ^. output) $* sy (glaTyFac ^. defLhs) $* sy loadSF)

{--}

pbIsSafe :: InstanceModel
pbIsSafe = imNoDeriv (equationalModelN (nounPhraseSP "Safety Req-Pb") pbIsSafeQD)
  [qwC probBr probConstraint, qwC pbTol probConstraint] isSafePb []
  [dRef astm2009] "isSafePb" [pbIsSafeDesc, probBRRef, pbTolUsr]

pbIsSafeQD :: SimpleQDef
pbIsSafeQD = mkQuantDef isSafePb (sy probBr $< sy pbTol)

{--}

lrIsSafe :: InstanceModel
lrIsSafe = imNoDeriv (equationalModelN (nounPhraseSP "Safety Req-LR") lrIsSafeQD)
  [qwC lRe $ UpFrom (Exc, exactDbl 0), qwC demand $ UpFrom (Exc, exactDbl 0)]
  isSafeLR []
  [dRef astm2009] "isSafeLR"
  [lrIsSafeDesc, capRef]

lrIsSafeQD :: SimpleQDef
lrIsSafeQD = mkQuantDef isSafeLR (sy lRe $> sy demand)

iModDesc :: DefinedQuantityDict -> Sentence -> Sentence
iModDesc main s = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
    S "considered safe", s `S.are` S "either both True or both False"]

-- Intro --

instModIntro :: Sentence
instModIntro = foldlSent [D.toSent $ atStartNP (the goal), refS willBreakGS,
  S "is met by", refS pbIsSafe `sC` refS lrIsSafe]

-- Notes --
lrCap :: Sentence
lrCap = ch lRe +:+. S "is also called capacity"

pbTolUsr :: Sentence
pbTolUsr = ch pbTol `S.is` S "entered by the" +:+. phrase user

lrIsSafeDesc :: Sentence
lrIsSafeDesc = iModDesc isSafeLR
  (ch isSafePb +:+ fromSource pbIsSafe `S.and_` ch isSafeLR)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = iModDesc isSafePb
  (ch isSafePb `S.and_` ch isSafeLR +:+ fromSource lrIsSafe)

capRef, ldfRef, nonFLRef, probBRRef, riskRef :: Sentence
capRef      = definedIn' calofCapacity (S "and is also called capacity")
ldfRef      = definedIn  loadDFDD
nonFLRef    = definedIn  nonFL
probBRRef   = definedIn  probOfBreak
riskRef     = definedIn  risk
