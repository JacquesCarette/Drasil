module Drasil.GlassBR.Unitals (module Drasil.GlassBR.Unitals) where --whole file is used

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators (parensNP)
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import Drasil.Database(mkUid)

import Prelude hiding (log)
import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Data.Drasil.Concepts.Math (xComp, yComp, zComp)
import Data.Drasil.Constraints (gtZeroConstr, probConstr)
import Data.Drasil.Quantities.Math (mathunitals, mathquants)
import Data.Drasil.Quantities.PhysicalProperties (physicalquants)
import Data.Drasil.Quantities.Physics (subMax, subMin, subX, subY, subZ)
import Data.Drasil.SI_Units (kilogram, metre, millimetre, pascal, second)

import Drasil.GlassBR.Concepts (annealedGl, aspectRatioCon, fTemperedGl, glTyFac,
  hStrengthGl, loadResis, loadShareFac, nonFactoredL, stdOffDist, loadDurFac, blast,
  blastResisGla, blastTy, bomb, capacity, demandq, eqTNTChar, explosion, glassGeo,
  glassTy, glassWL, glBreakage, lateral, lite, load, longDurLoad, modE, notSafe,
  probBreak, safeMessage, shortDurLoad, specA, specDeLoad, glassType)
import Drasil.GlassBR.Units (sFlawPU)

symbols :: [DefinedQuantityDict]
symbols = NE.toList inputs ++ tmSymbols ++ map dqdWr specParamVals ++
  [modElas] ++ interps ++ unitalSymbols ++
  unitless ++ map dqdWr [probBr, stressDistFac] ++
  map dqdWr derivedInputDataConstraints ++
  mathunitals ++ physicalquants ++ mathquants

constrained :: [ConstrConcept]
constrained = map cnstrw' dataConstraints ++ map cnstrw' [nomThick, glassTypeCon]

plateLen, plateWidth, aspectRatio, charWeight, standOffDist :: UncertQ
pbTol, tNT :: UncertQ
glassTypeCon, nomThick :: ConstrConcept

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = NE.map dqdWr inputsWUnitsUncrtn <> NE.map dqdWr inputsWUncrtn <>
  NE.map dqdWr inputsNoUncrtn <> sdVector

--inputs with units and uncertainties
inputsWUnitsUncrtn :: NE.NonEmpty UncertQ
inputsWUnitsUncrtn = plateLen :| [plateWidth, charWeight]

--inputs with uncertainties and no units
inputsWUncrtn :: NE.NonEmpty UncertQ
inputsWUncrtn = pbTol :| [tNT]

--inputs with no uncertainties
inputsNoUncrtn :: NE.NonEmpty ConstrConcept
inputsNoUncrtn = NE.map cnstrw' $ glassTypeCon :| [nomThick]

--derived inputs with units and uncertainties
derivedInsWUnitsUncrtn :: [UncertQ]
derivedInsWUnitsUncrtn = [standOffDist]

--derived inputs with uncertainties and no units
derivedInsWUncrtn :: [UncertQ]
derivedInsWUncrtn = [aspectRatio]

inputDataConstraints :: [UncertQ]
inputDataConstraints = NE.toList $ inputsWUnitsUncrtn <> inputsWUncrtn

derivedInputDataConstraints :: [UncertQ]
derivedInputDataConstraints = derivedInsWUnitsUncrtn
  ++ derivedInsWUncrtn

dataConstraints :: [UncertQ]
dataConstraints = inputDataConstraints ++ derivedInputDataConstraints

plateLen = uqc "plateLen" (nounPhraseSP "plate length (long dimension)")
  "the length (long dimension) of the glass plate" lA metre Real
  [ gtZeroConstr,
    physRange $ UpFrom (Inc, sy plateWidth),
    sfwrRange $ Bounded (Inc , sy dimMin) (Inc , sy dimMax)] (dbl 1.5) defaultUncrt

plateWidth = uqc "plateWidth" (nounPhraseSP "plate width (short dimension)")
  "the width (short dimension) of the glass plate" lB metre Real
  [ physRange $ Bounded (Exc, exactDbl 0) (Inc, sy plateLen),
    sfwrRange $ Bounded (Inc, sy dimMin) (Inc, sy dimMax)] (dbl 1.2) defaultUncrt

aspectRatio = uq (constrained' (dqdNoUnit aspectRatioCon (variable "AR") Real)
  [ physRange $ UpFrom (Inc, exactDbl 1),
    sfwrRange $ UpTo (Inc, sy arMax)] (dbl 1.5)) defaultUncrt

pbTol = uq (constrained' (quantNoUnit (mkUid "pbTol") (nounPhraseSP "tolerable probability of breakage")
  (S "the tolerable probability of breakage of the glass plate")
  (sub cP (Concat [lBreak, lTol])) Real)
  [probConstr] (dbl 0.008)) (uncty 0.001 Nothing)

charWeight = uqcND "charWeight" (nounPhraseSP "charge weight")
  lW kilogram Real
  [ gtZeroConstr,
    sfwrRange $ Bounded (Inc, sy cWeightMin) (Inc, sy cWeightMax)]
    (exactDbl 42) defaultUncrt

tNT = uq (constrained' (quantNoUnit (mkUid "tNT") (nounPhraseSP "TNT equivalent factor")
  (S "the TNT equivalent factor")
  (variable "TNT") Real)
  [ gtZeroConstr ] (exactDbl 1)) defaultUncrt

standOffDist = uq (constrained' (dqd stdOffDist (variable "SD") Real metre)
  [ gtZeroConstr,
    sfwrRange $ Bounded (Inc, sy sdMin) (Inc, sy sdMax)] (exactDbl 45)) defaultUncrt

nomThick = cuc' "nomThick" (nounPhraseSP "nominal thickness")
  "the specified standard thickness of the glass plate" lT millimetre
  {-Discrete nominalThicknesses, but not implemented-} Rational
  [sfwrElem $ mkSet Rational (map dbl nominalThicknesses)] $ exactDbl 8 -- for testing

glassTypeCon = constrainedNRV' (dqdNoUnit glassTy lG String)
  [sfwrElem $ mkSet String $ map (str . abrv . snd) glassType]

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = (isSafePb :| [isSafeLR]) <> NE.map dqdWr (probBr :| [stressDistFac])

-- | Symbols uniquely relevant to theory models.
tmSymbols :: [DefinedQuantityDict]
tmSymbols = map dqdWr [probFail, pbTolfail]

probBr, probFail, pbTolfail, stressDistFac :: ConstrConcept
probBr = constrained' (dqdNoUnit probBreak
  (sub cP lBreak) Real)
  [probConstr] (dbl 0.4)

stressDistFac = cucNoUnit' "stressDistFac" (nounPhraseSP "stress distribution factor (Function)")
  "the stress distribution factor of the glass plate"
  cJ Real [physRange $ Bounded (Inc, sy stressDistFacMin)
  (Inc, sy stressDistFacMax)] (exactDbl 15)

probFail = cucNoUnit' "probFail" (nounPhraseSP "probability of failure")
  "the probability of failure of the glass plate"
  (sub cP lFail) Real
  [probConstr] (dbl 0.4)

pbTolfail = cucNoUnit' "pbTolfail" (nounPhraseSP "tolerable probability of failure")
  "the tolerable probability of failure of the glass plate"
  (sub cP (Concat [lFail, lTol])) Real
  [probConstr] (dbl 0.008)

  --FIXME: no typical value!

specParamVals :: [ConstQDef]
specParamVals = [dimMax, dimMin, arMax, cWeightMax, cWeightMin,
  sdMax, sdMin, stressDistFacMin, stressDistFacMax]

dimMax, dimMin, arMax, cWeightMax, cWeightMin, sdMax, stressDistFacMin, stressDistFacMax,
  sdMin :: ConstQDef

dimMax     = mkQuantDef (quant (mkUid "dimMax")
  (nounPhraseSP "maximum value for one of the dimensions of the glass plate")
  (S "the maximum value for one of the dimensions of the glass plate")
  (subMax lD) Real metre) (exactDbl 5)

dimMin     = mkQuantDef (quant (mkUid "dimMin")
  (nounPhraseSP "minimum value for one of the dimensions of the glass plate")
  (S "the minimum value for one of the dimensions of the glass plate")
  (subMin lD) Real metre) (dbl 0.1)

arMax     = mkQuantDef (quantNoUnit (mkUid "arMax")
  (nounPhraseSP "maximum aspect ratio")
  (S "the maximum aspect ratio")
  (subMax (variable "AR")) Real) (exactDbl 5)

cWeightMax = mkQuantDef (quant (mkUid "cWeightMax")
  (nounPhraseSP "maximum permissible input charge weight")
  (S "the maximum permissible input charge weight")
  (subMax (eqSymb charWeight)) Real kilogram) (exactDbl 910)

cWeightMin = mkQuantDef (quant (mkUid "cWeightMin")
  (nounPhraseSP "minimum permissible input charge weight")
  (S "the minimum permissible input charge weight")
  (subMin (eqSymb charWeight)) Real kilogram) (dbl 4.5)

sdMax     = mkQuantDef (quant (mkUid "sdMax")
  (nounPhraseSP "maximum stand off distance permissible for input")
  (S "the maximum stand off distance permissible for input")
  (subMax (eqSymb standOffDist)) Real metre) (exactDbl 130)

sdMin     = mkQuantDef (quant (mkUid "sdMin")
  (nounPhraseSP "minimum stand off distance permissible for input")
  (S "the minimum stand off distance permissible for input")
  (subMin (eqSymb standOffDist)) Real metre) (exactDbl 6)

stressDistFacMin = mkQuantDef (quantNoUnit (mkUid "stressDistFacMin")
  (nounPhraseSP "minimum value for the stress distribution factor")
  (S "the minimum value for the stress distribution factor")
  (subMin (eqSymb stressDistFac)) Real) (exactDbl 1)

stressDistFacMax = mkQuantDef (quantNoUnit (mkUid "stressDistFacMax")
  (nounPhraseSP "maximum value for the stress distribution factor")
  (S "the maximum value for the stress distribution factor")
  (subMax (eqSymb stressDistFac)) Real) (exactDbl 32)

unitalSymbols :: [DefinedQuantityDict]
unitalSymbols = [demand, tmDemand, lRe, tmLRe, nonFactorL, eqTNTWeight,
  sflawParamK, sflawParamM, loadDur, minThick]

sdx, sdy, sdz :: DefinedQuantityDict

demand, tmDemand, lRe, tmLRe, minThick, nonFactorL, eqTNTWeight,
  sflawParamM, sflawParamK, loadDur, modElas :: DefinedQuantityDict

demand      = dqd demandq lQ Real pascal --correct Space used?

tmDemand    = dqd load (variable "Load") Real pascal --correct Space used?

lRe         = dqd loadResis (variable "LR") Real pascal --correct Space used?

tmLRe       = dqd capacity (variable "capacity") Real pascal --correct Space used?

nonFactorL  = dqd nonFactoredL (variable "NFL") Real pascal --correct Space used?

eqTNTWeight = dqd eqTNTChar (sub (eqSymb charWeight) (eqSymb tNT)) Real
  kilogram

modElas     = dqd modE cE Real pascal

minThick    = quant (mkUid "minThick") (nounPhraseSP "minimum thickness")
  (S "minimum thickness of the glass plate") lH Real metre

sflawParamK = quant (mkUid "sflawParamK") (nounPhraseSP "surface flaw parameter") --parameterize?
  (S ("surface flaw parameter related to the coefficient of " ++
    "variation of the glass strength data")) lK Real sFlawPU

sflawParamM = quant (mkUid "sflawParamM") (nounPhraseSP "surface flaw parameter") --parameterize?
  (S "surface flaw parameter related to the mean of the glass strength data")
  lM Real sFlawPU

loadDur     = quant (mkUid "loadDur")    (nounPhraseSP "duration of load")
  (S "the amount of time that a load is applied to the glass plate")
  (sub lT lDur) Real second

sdx         = quant (mkUid "sdx") (compoundPhrase (standOffDist ^. term) (parensNP (xComp ^. term)))
  (S "the x-component of the stand off distance") (subX (eqSymb standOffDist)) Real metre

sdy         = quant (mkUid "sdy") (compoundPhrase (standOffDist ^. term) (parensNP (yComp ^. term)))
  (S "the y-component of the stand off distance") (subY (eqSymb standOffDist)) Real metre

sdz         = quant (mkUid "sdz") (compoundPhrase (standOffDist ^. term) (parensNP (zComp ^. term)))
  (S "the x-component of the stand off distance") (subZ (eqSymb standOffDist)) Real metre

{-Quantities-}

unitless :: [DefinedQuantityDict]
unitless = [riskFun, isSafePb, isSafeProb, isSafeLR, isSafeLoad,
  sdfTol, dimlessLoad, tolLoad, gTF, loadSF, loadDF]

interps :: [DefinedQuantityDict]
interps = [interpY, interpZ]

riskFun, isSafePb, isSafeProb, isSafeLR, isSafeLoad, sdfTol,
  dimlessLoad, tolLoad, interpY, interpZ :: DefinedQuantityDict

gTF, loadSF, loadDF :: DefinedQuantityDict

dimlessLoad = quantNoUnit (mkUid "dimlessLoad") (nounPhraseSP "dimensionless load")
  (S "the dimensionless load") (hat lQ) Real

gTF           = dqdNoUnit glTyFac (variable "GTF") Integer

isSafePb   = quantNoUnit (mkUid "isSafePb") (nounPhraseSP "probability of glass breakage safety requirement")
  (S "the probability of glass breakage safety requirement") (variable "isSafePb") Boolean
isSafeProb = quantNoUnit (mkUid "isSafeProb") (nounPhraseSP "probability of failure safety requirement")
  (S "the probability of failure safety requirement") (variable "isSafeProb") Boolean
isSafeLR   = quantNoUnit (mkUid "isSafeLR")   (nounPhraseSP "3 second load equivalent resistance safety requirement")
  (S "the 3 second load equivalent resistance safety requirement") (variable "isSafeLR") Boolean
isSafeLoad = quantNoUnit (mkUid "isSafeLoad") (nounPhraseSP "load resistance safety requirement")
  (S "the load resistance safety requirement") (variable "isSafeLoad") Boolean

interpY = quantNoUnit (mkUid "interpY") (nounPhraseSP "interpY")
  (S "interpolated y") (variable "interpY") (mkFunction [String, Real, Real] Real)
interpZ = quantNoUnit (mkUid "interpZ") (nounPhraseSP "interpZ")
  (S "interpolated z") (variable "interpZ") (mkFunction [String, Real, Real] Real)

loadDF        = dqdNoUnit loadDurFac (variable "LDF") Real
loadSF        = dqdNoUnit loadShareFac (variable "LSF") Real

riskFun = quantNoUnit (mkUid "riskFun") (nounPhraseSP "risk of failure")
  (S "the percentage risk of the glass slab failing to resist the blast") cB Real

sdfTol = quantNoUnit (mkUid "sdfTol") (nounPhraseSP "tolerable stress distribution factor")
  (S "the tolerable stress distribution factor") (sub (eqSymb stressDistFac) lTol) Real

tolLoad = quantNoUnit (mkUid "tolLoad") (nounPhraseSP "tolerable load")
  (S "the tolerable load") (sub (eqSymb dimlessLoad) lTol) Real

lBreak, lDur, lFail, lTol :: Symbol
lBreak = label "b"
lDur   = label "d"
lFail  = label "f"
lTol   = label "tol"

concepts :: [ConceptChunk]
concepts = [glBreakage, lite, annealedGl, fTemperedGl, hStrengthGl, lateral,
  specDeLoad, longDurLoad, glassWL, shortDurLoad, specA, blastResisGla, blast,
  blastTy, glassGeo, safeMessage, notSafe, bomb, explosion]

--Constants--

constants :: [ConstQDef]
constants = [constantM, constantK, constantModElas, constantLoadDur, constantLoadSF]
                ++ specParamVals

constantM, constantK, constantModElas, constantLoadDur, constantLoadSF :: ConstQDef
constantM       = mkQuantDef sflawParamM $ exactDbl 7
constantK       = mkQuantDef sflawParamK $ dbl 2.86e-53
constantModElas = mkQuantDef modElas     $ dbl 7.17e10
constantLoadDur = mkQuantDef loadDur     $ exactDbl 3
constantLoadSF  = mkQuantDef loadSF      $ exactDbl 1

--Equations--

sdVector :: NE.NonEmpty DefinedQuantityDict
sdVector = sdx :| [sdy, sdz]

--
--Pulled to be used in "Terms And Definitions" Section--
termsWithDefsOnly, termsWithAccDefn, loadTypes, glassTypes :: [ConceptChunk]

glassTypes = [annealedGl, fTemperedGl, hStrengthGl]
termsWithDefsOnly = [glBreakage, lateral, lite, specA, blastResisGla, eqTNTChar]
termsWithAccDefn  = [stdOffDist, loadShareFac, glTyFac, aspectRatioCon]
loadTypes = [loadResis, nonFactoredL, glassWL, shortDurLoad, specDeLoad, longDurLoad]

--Defined for DataDefs.hs and this file only--
actualThicknesses :: [Double]
actualThicknesses = map snd glassThickness

nominalThicknesses :: [Double]
nominalThicknesses = map fst glassThickness

glassTypeFactors :: [Integer]
glassTypeFactors = map fst glassType

type GlassThickness = [(Double, Double)] --[(Nominal, Actual)]

glassThickness :: GlassThickness
glassThickness =
  [(2.5, 2.16),
  (2.7, 2.59),
  (3.0, 2.92),
  (4.0, 3.78),
  (5.0, 4.57),
  (6.0, 5.56),
  (8.0, 7.42),
  (10.0, 9.02),
  (12.0, 11.91),
  (16.0, 15.09),
  (19.0, 18.26),
  (22.0, 21.44)]

--Below are present in this file temporarily--
lateralLoad :: IdeaDict
lateralLoad = compoundNC lateral load
