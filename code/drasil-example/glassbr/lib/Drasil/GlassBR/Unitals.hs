module Drasil.GlassBR.Unitals (module Drasil.GlassBR.Unitals) where --whole file is used

import Language.Drasil
import Drasil.NaturalLanguage.English.NounPhrase.Combinators (parensNP)
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import Drasil.Database(mkUid)
import Drasil.GlassBR.References (astm2009, astm2016)
import Language.Drasil.Document (refS, complexRef)

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

import Drasil.GlassBR.Concepts (annealedGl, fTemperedGl, hStrengthGl, iGlass, lGlass,
  blast, blastResisGla, blastTy, bomb, explosion, glassGeo,
  glassWL, glBreakage, lateral, lite, longDurLoad, notSafe,
  safeMessage, shortDurLoad, specA, specDeLoad, glassType)
import Drasil.GlassBR.Units (sFlawPU)

symbols :: [DefinedQuantityDict]
symbols = NE.toList inputs <> tmSymbols <> fmap dqdWr specParamVals <>
  [modElas] <> interps <> unitalSymbols <>
  unitless <> [probBr, stressDistFac] <>
  [standOffDist, aspectRatio] <>
  mathunitals <> physicalquants <> mathquants

constrained :: [ConstrConcept]
constrained = fmap cnstrw' dataConstraints <> [nomThickCon, glassTypeCon]

plateLen, plateWidth, aspectRatio, charWeight, standOffDist, pbTol, tNT,
  nomThick, glassTypeQ :: DefinedQuantityDict
plateLenUnc, plateWidthUnc, aspectRatioUnc, charWeightUnc, standOffDistUnc,
  pbTolUnc, tNTUnc :: UncertQ
glassTypeCon, nomThickCon :: ConstrConcept

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = (plateLen :| [plateWidth, charWeight, pbTol, tNT, glassTypeQ, nomThick]) <> sdVector

--inputs with units and uncertainties
inputsWUnitsUncrtn :: NE.NonEmpty UncertQ
inputsWUnitsUncrtn = plateLenUnc :| [plateWidthUnc, charWeightUnc]

--inputs with uncertainties and no units
inputsWUncrtn :: NE.NonEmpty UncertQ
inputsWUncrtn = pbTolUnc :| [tNTUnc]

--inputs with no uncertainties
inputsNoUncrtn :: NE.NonEmpty ConstrConcept
inputsNoUncrtn = glassTypeCon :| [nomThickCon]

--derived inputs with units and uncertainties
derivedInsWUnitsUncrtn :: [UncertQ]
derivedInsWUnitsUncrtn = [standOffDistUnc]

--derived inputs with uncertainties and no units
derivedInsWUncrtn :: [UncertQ]
derivedInsWUncrtn = [aspectRatioUnc]

inputDataConstraints :: [UncertQ]
inputDataConstraints = NE.toList $ inputsWUnitsUncrtn <> inputsWUncrtn

derivedInputDataConstraints :: [UncertQ]
derivedInputDataConstraints = derivedInsWUnitsUncrtn
  <> derivedInsWUncrtn

dataConstraints :: [UncertQ]
dataConstraints = inputDataConstraints <> derivedInputDataConstraints

plateLen = quant (mkUid "plateLen") (nounPhraseSP "plate length (long dimension)")
  (S "the length (long dimension) of the glass plate") lA Real metre

plateLenUnc = uq (constrained' plateLen
  [ gtZeroConstr,
    physRange $ UpFrom (Inc, sy plateWidth),
    sfwrRange $ Bounded (Inc , sy dimMin) (Inc , sy dimMax)] (dbl 1.5)) defaultUncrt

plateWidth = quant (mkUid "plateWidth") (nounPhraseSP "plate width (short dimension)")
  (S "the width (short dimension) of the glass plate") lB Real metre

plateWidthUnc = uq (constrained' plateWidth
  [ physRange $ Bounded (Exc, exactDbl 0) (Inc, sy plateLen),
    sfwrRange $ Bounded (Inc, sy dimMin) (Inc, sy dimMax)] (dbl 1.2)) defaultUncrt

aspectRatio = quantNoUnit (mkUid "aR") (nounPhraseSP "aspect ratio")
  (S $ "the ratio of the long dimension of the glass to the short dimension of " <>
    "the glass. For glass supported on four sides, the aspect ratio is " <>
    "always equal to or greater than 1.0. For glass supported on three " <>
    "sides, the ratio of the length of one of the supported edges " <>
    "perpendicular to the free edge, to the length of the free edge, is " <>
    "equal to or greater than 0.5")
  (variable "AR") Real

aspectRatioUnc = uq (constrained' aspectRatio
  [ physRange $ UpFrom (Inc, exactDbl 1),
    sfwrRange $ UpTo (Inc, sy arMax)] (dbl 1.5)) defaultUncrt

pbTol = quantNoUnit (mkUid "pbTol") (nounPhraseSP "tolerable probability of breakage")
  (S "the tolerable probability of breakage of the glass plate")
  (sub cP (lBreak <> lTol)) Real

pbTolUnc = uq (constrained' pbTol
  [probConstr] (dbl 0.008)) (uncty 0.001 Nothing)

charWeight = quant (mkUid "charWeight") (nounPhraseSP "charge weight")
  (S "") lW Real kilogram

charWeightUnc = uq (constrained' charWeight
  [ gtZeroConstr,
    sfwrRange $ Bounded (Inc, sy cWeightMin) (Inc, sy cWeightMax)]
    (exactDbl 42)) defaultUncrt

tNT = quantNoUnit (mkUid "tNT") (nounPhraseSP "TNT equivalent factor")
  (S "the TNT equivalent factor")
  (variable "TNT") Real

tNTUnc = uq (constrained' tNT
  [ gtZeroConstr ] (exactDbl 1)) defaultUncrt

standOffDist = quant (mkUid "stdOffDist") (nounPhraseSP "stand off distance")
  (S "the distance from the glazing surface to the centroid of a hemispherical" +:+
   S "high explosive charge")
  (variable "SD") Real metre

standOffDistUnc = uq (constrained' standOffDist
  [ gtZeroConstr,
    sfwrRange $ Bounded (Inc, sy sdMin) (Inc, sy sdMax)] (exactDbl 45)) defaultUncrt

nomThick = quant (mkUid "nomThick") (nounPhraseSP "nominal thickness")
  (S "the specified standard thickness of the glass plate") lT
  {-Discrete nominalThicknesses, but not implemented-} Rational millimetre

nomThickCon = constrained' nomThick
  [sfwrElem $ mkSet Rational (dbl <$> nominalThicknesses)] $ exactDbl 8 -- for testing

glassTypeQ = quantNoUnit' (mkUid "glassTy")
  (cn' "glass type")
  (S "type of glass")
  (const lG) String

glassTypeCon = constrainedNRV' glassTypeQ
  [sfwrElem $ mkSet String $ str . abrv . snd <$> glassType]

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = isSafePb :| [isSafeLR, probBr, stressDistFac]

-- | Symbols uniquely relevant to theory models.
tmSymbols :: [DefinedQuantityDict]
tmSymbols = dqdWr <$> [probFail, pbTolfail]

probBr, stressDistFac :: DefinedQuantityDict
probBrCon, probFail, pbTolfail, stressDistFacCon :: ConstrConcept
probBr = quantNoUnit' (mkUid "probBr")
  (nounPhraseSP "probability of breakage")
  (foldlSent_ [S "the fraction of glass lites or plies that would break at the",
    S "first occurrence of a specified load and duration, typically expressed",
    S "in lites per 1000", sParen $ refS astm2016])
  (const $ sub cP lBreak) Real

probBrCon = constrained' probBr [probConstr] (dbl 0.4)

stressDistFac = quantNoUnit (mkUid "stressDistFac")
  (nounPhraseSP "stress distribution factor (Function)")
  (S "the stress distribution factor of the glass plate") cJ Real

stressDistFacCon = constrained' stressDistFac
  [physRange $ Bounded (Inc, sy stressDistFacMin)
  (Inc, sy stressDistFacMax)] (exactDbl 15)

probFail = cucNoUnit' "probFail" (nounPhraseSP "probability of failure")
  "the probability of failure of the glass plate"
  (sub cP lFail) Real
  [probConstr] (dbl 0.4)

pbTolfail = cucNoUnit' "pbTolfail" (nounPhraseSP "tolerable probability of failure")
  "the tolerable probability of failure of the glass plate"
  (sub cP (lFail <> lTol)) Real
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

demand      = quant (mkUid "demandq") (nounPhraseSP "applied load (demand)")
  (S "3 second duration equivalent pressure") lQ Real pascal --correct Space used?

tmDemand    = quant (mkUid "load") (nounPhraseSP "applied load (demand) or pressure")
  (S "a uniformly distributed lateral pressure") (variable "Load") Real pascal --correct Space used?

lRe         = quant (mkUid "lResistance") (nounPhraseSP "load resistance")
  (foldlSent_ [S "the uniform lateral load that a glass construction can sustain",
  S "based upon a given probability of breakage and load duration as defined in",
  complexRef astm2009 $ Page [1, 53]])
  (variable "LR") Real pascal --correct Space used?

tmLRe       = quant (mkUid "capacity") (nounPhraseSP "capacity or load resistance")
  (S "load resistance calculated") (variable "capacity") Real pascal --correct Space used?

nonFactorL  = quant (mkUid "nFL") (nounPhraseSP "non-factored load")
  (foldlSent_ [S "three second duration uniform load associated with a",
  S "probability of breakage less than or equal to 8", plural lite,
  S "per 1000 for monolithic", short annealedGl, S "glass"])
  (variable "NFL") Real pascal --correct Space used?

eqTNTWeight = quant (mkUid "eqTNTChar") (nounPhraseSP "equivalent TNT charge mass")
  (S "mass of TNT placed on the ground in a hemisphere that represents the design explosive threat")
  (sub (eqSymb charWeight) (eqSymb tNT)) Real kilogram

modElas     = quant (mkUid "modElas") (nounPhraseSP "modulus of elasticity of glass")
  (S "the ratio of tensile stress to tensile strain of glass") cE Real pascal

minThick    = quant (mkUid "minThick") (nounPhraseSP "minimum thickness")
  (S "minimum thickness of the glass plate") lH Real metre

sflawParamK = quant (mkUid "sflawParamK") (nounPhraseSP "surface flaw parameter") --parameterize?
  (S ("surface flaw parameter related to the coefficient of " <>
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

gTF = quantNoUnit (mkUid "glassTypeFac") (nounPhraseSP "glass type factor")
  (foldlSent_ [S "a multiplying factor for adjusting the", phrase lRe,
  S "of different glass type, that is,",
  foldlList Comma Options (short <$> [annealedGl, fTemperedGl, hStrengthGl])
  `sC` S "in monolithic glass" `sC` short lGlass, sParen (titleize lGlass) `sC`
   S "or", short iGlass, sParen (titleize iGlass), S "constructions"])
  (variable "GTF") Integer

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

loadDF = quantNoUnit (mkUid "loadDurFactor") (nounPhraseSP "load duration factor")
  (S "factor related to the effect of sustained loading on glass strength")
  (variable "LDF") Real
loadSF = quantNoUnit (mkUid "lShareFac") (nounPhraseSP "load share factor")
  (foldlSent_ [S "a multiplying factor derived from the load sharing between the",
  S "double glazing, of equal or different thicknesses and types (including the",
  S "layered behaviour of", short lGlass, S "under long duration",
  S "loads), in a sealed", short iGlass, S "unit"])
  (variable "LSF") Real

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
                <> specParamVals

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
termsWithDefsOnly, loadTypes, glassTypes :: [ConceptChunk]

glassTypes = [annealedGl, fTemperedGl, hStrengthGl]
termsWithDefsOnly = [glBreakage, lateral, lite, specA, blastResisGla]
loadTypes = [glassWL, shortDurLoad, specDeLoad, longDurLoad]

--Defined for DataDefs.hs and this file only--
actualThicknesses :: [Double]
actualThicknesses = snd <$> glassThickness

nominalThicknesses :: [Double]
nominalThicknesses = fst <$> glassThickness

glassTypeFactors :: [Integer]
glassTypeFactors = fst <$> glassType

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
lateralLoad = compoundNC lateral tmDemand
