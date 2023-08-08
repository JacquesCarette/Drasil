module Drasil.GlassBR.DataDefs (dataDefs, glaTyFac, glaTyFacQD, gtfRef, hFromt,
  hFromtQD, hRef, loadDF, standOffDis, eqTNTWDD, calofDemand, configFp,
  stdVals) where

import Control.Lens ((^.))
import Language.Drasil
import Prelude hiding (log, exp, sqrt)
import Theory.Drasil (DataDefinition, ddE)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Math (parameter)

import Drasil.GlassBR.Assumptions (assumpSV, assumpLDFC)
import Drasil.GlassBR.Figures (demandVsSDFig)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (actualThicknesses, charWeight, demand, demandq,
  eqTNTWeight, gTF, glassType, glassTypeCon, glassTypeFactors, interpY,
  lDurFac, loadDur, minThick, nomThick, nominalThicknesses, sdx, sdy, sdz,
  sflawParamM, standOffDist, tNT)
import Drasil.GlassBR.Concepts (annealed, fullyT, glass, heatS)

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefs :: [DataDefinition]
dataDefs = [hFromt, loadDF, glaTyFac, standOffDis, eqTNTWDD, calofDemand]

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
glaTyFacEq = incompleteCase (zipWith glaTyFacHelper glassTypeFactors $ map (getAccStr . snd) glassType)

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

--Additional Notes--
anGlass :: Sentence
anGlass = getAcc annealed `S.is` phrase annealed +:+. phrase glass

ftGlass :: Sentence
ftGlass = getAcc fullyT `S.is` phrase fullyT +:+. phrase glass

hsGlass :: Sentence
hsGlass = getAcc heatS `S.is` phrase heatS +:+. phrase glass

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

gtfRef, hRef :: Sentence
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
