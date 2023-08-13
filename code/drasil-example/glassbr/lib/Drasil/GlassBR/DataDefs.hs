module Drasil.GlassBR.DataDefs (dataDefs, aspRat, glaTyFac, glaTyFacQD, gtfRef,
  hFromt, hFromtQD, loadDF, standOffDis, eqTNTWDD, calofDemand, aGrtrThanB,
  arRef, hRef, configFp, stdVals) where

import Control.Lens ((^.))
import Language.Drasil
import Prelude hiding (log, exp, sqrt)
import Theory.Drasil (DataDefinition, ddE)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.PhysicalProperties (dimension)

import Drasil.GlassBR.Assumptions (assumpSV, assumpLDFC)
import Drasil.GlassBR.Concepts (annealed, fullyT, glass, heatS)
import Drasil.GlassBR.Figures (demandVsSDFig)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefs :: [DataDefinition]
dataDefs = [hFromt, loadDF, glaTyFac, standOffDis, aspRat, eqTNTWDD, calofDemand]

{--}

hFromtEq :: Relation
hFromtEq = frac 1 1000 `mulRe` incompleteCase (zipWith hFromtHelper
  actualThicknesses nominalThicknesses)

hFromtHelper :: Double -> Double -> (Expr, Relation)
hFromtHelper result condition = (dbl result, sy nomThick $= dbl condition)

hFromtQD :: SimpleQDef
hFromtQD = mkQuantDef minThick hFromtEq

hFromt :: DataDefinition
hFromt = ddE hFromtQD [dRef astm2009] Nothing "minThick" [hMin]

{--}

loadDFEq :: Expr
loadDFEq = (sy loadDur $/ exactDbl 60) $^ (sy sflawParamM $/ exactDbl 16)

loadDFQD :: SimpleQDef
loadDFQD = mkQuantDef lDurFac loadDFEq

loadDF :: DataDefinition
loadDF = ddE loadDFQD [dRef astm2009] Nothing "loadDurFactor"
  [stdVals [loadDur, sflawParamM], ldfConst]

{--}

glaTyFacEq :: Expr
glaTyFacEq = incompleteCase (zipWith glaTyFacHelper glassTypeFactors $ map (abrv . snd) glassType)

glaTyFacHelper :: Integer -> String -> (Expr, Relation)
glaTyFacHelper result condition = (int result, sy glassTypeCon $= str condition)

glaTyFacQD :: SimpleQDef
glaTyFacQD = mkQuantDef gTF glaTyFacEq

glaTyFac :: DataDefinition
glaTyFac = ddE glaTyFacQD [dRef astm2009] Nothing "gTF"
  [anGlass, ftGlass, hsGlass]

{--}

standOffDisEq :: Expr
standOffDisEq = sqrt (square (sy sdx) `addRe` square (sy sdy) `addRe` square (sy sdz))

standOffDisQD :: SimpleQDef
standOffDisQD = mkQuantDef standOffDist standOffDisEq

standOffDis :: DataDefinition
standOffDis = ddE standOffDisQD [dRef astm2009] Nothing "standOffDist" []

{--}

aspRatEq :: Expr
aspRatEq = sy plateLen $/ sy plateWidth

aspRatQD :: SimpleQDef
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRat :: DataDefinition
aspRat = ddE aspRatQD [dRef astm2009] Nothing "aspectRatio" [aGrtrThanB]

{--}

eqTNTWEq :: Expr
eqTNTWEq = mulRe (sy charWeight) (sy tNT)

eqTNTWQD :: SimpleQDef
eqTNTWQD = mkQuantDef eqTNTWeight eqTNTWEq

eqTNTWDD :: DataDefinition
eqTNTWDD = ddE eqTNTWQD [dRef astm2009] Nothing "eqTNTW" []

{--}

calofDemandEq :: Expr
calofDemandEq = apply interpY [str "TSD.txt", sy standOffDist, sy eqTNTWeight]

calofDemandQD :: SimpleQDef
calofDemandQD = mkQuantDef demand calofDemandEq

calofDemand :: DataDefinition
calofDemand = ddE calofDemandQD [dRef astm2009] Nothing "calofDemand" [calofDemandDesc]

-- Additional Notes --
aGrtrThanB :: Sentence
aGrtrThanB = ch plateLen `S.and_` ch plateWidth `S.are`
  (plural dimension `S.the_ofThe` S "plate") `sC` S "where" +:+.
  sParen (eS $ sy plateLen $>= sy plateWidth)

anGlass, ftGlass, hsGlass :: Sentence
anGlass = glassTypeHelper annealed
ftGlass = glassTypeHelper fullyT
hsGlass = glassTypeHelper heatS

glassTypeHelper :: CI -> Sentence
glassTypeHelper t = getAcc t `S.is` phrase t +:+. phrase glass

calofDemandDesc :: Sentence
calofDemandDesc =
  foldlSent [ch demand `sC` EmptyS `S.or_` phrase demandq `sC` EmptyS `S.isThe`
  (demandq ^. defn), S "obtained from", refS demandVsSDFig,
  S "by interpolation using", phrase standOffDist, sParen (ch standOffDist)
  `S.and_` ch eqTNTWeight, S "as" +:+. plural parameter, ch eqTNTWeight,
  S "is defined in" +:+. refS eqTNTWDD, ch standOffDist `S.isThe`
  phrase standOffDist, S "as defined in", refS standOffDis]

hMin :: Sentence
hMin = ch nomThick `S.is` S "a function that maps from the nominal thickness"
  +:+. (sParen (ch minThick) `S.toThe` phrase minThick)

ldfConst :: Sentence
ldfConst = ch lDurFac `S.is` S "assumed to be constant" +:+. fromSource assumpLDFC

arRef, gtfRef, hRef :: Sentence
arRef  = definedIn  aspRat
gtfRef = definedIn  glaTyFac
hRef   = definedIn' hFromt (S "and is based on the nominal thicknesses")

-- List of Configuration Files necessary for DataDefs.hs
configFp :: [String]
configFp = ["SDF.txt", "TSD.txt"]

--- Helper
stdVals :: (HasSymbol s, HasUID s) => [s] -> Sentence
stdVals s = foldlList Comma List (map ch s) +:+ sent +:+. refS assumpSV
  where sent = case s of [ ]   -> error "stdVals needs quantities"
                         [_]   -> S "comes from"
                         (_:_) -> S "come from"
