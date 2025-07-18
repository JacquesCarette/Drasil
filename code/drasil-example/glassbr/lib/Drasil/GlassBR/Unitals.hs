module Drasil.GlassBR.Unitals where --whole file is used

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators

import Prelude hiding (log)

import Data.Drasil.Concepts.Math (xComp, yComp, zComp)
import Data.Drasil.Constraints (gtZeroConstr, probConstr)
import Data.Drasil.Quantities.Physics (subMax, subMin, subX, subY, subZ)
import Data.Drasil.SI_Units (kilogram, metre, millimetre, pascal, second)

import Drasil.GlassBR.Concepts (aR, annealed, fullyT, glaPlane, glassTypeFac,
  heatS, iGlass, lGlass, lResistance, lShareFac, nFL, responseTy,
  stdOffDist, lDurFac)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016)
import Drasil.GlassBR.Units (sFlawPU)
--FIXME: Many of the current terms can be separated into terms and defns?

{--}

constrained :: [ConstrainedChunk]
constrained = map cnstrw dataConstraints ++ map cnstrw [nomThick, glassTypeCon]
 -- map cnstrw inputDataConstraints ++ map cnstrw derivedInputDataConstraints -- ++
  -- [cnstrw probBr, cnstrw probFail, cnstrw stressDistFac, cnstrw nomThick, cnstrw glassTypeCon]

plateLen, plateWidth, aspectRatio, charWeight, standOffDist :: UncertQ
pbTol, tNT :: UncertainChunk
glassTypeCon, nomThick :: ConstrConcept

{--}

inputs :: [QuantityDict]
inputs = map qw inputsWUnitsUncrtn ++ map qw inputsWUncrtn ++
  map qw inputsNoUncrtn ++ map qw sdVector

--inputs with units and uncertainties
inputsWUnitsUncrtn :: [UncertQ]
inputsWUnitsUncrtn = [plateLen, plateWidth, charWeight]

--inputs with uncertainties and no units
inputsWUncrtn :: [UncertainChunk]
inputsWUncrtn = [pbTol, tNT]

--inputs with no uncertainties
inputsNoUncrtn :: [ConstrainedChunk]
inputsNoUncrtn = map cnstrw [glassTypeCon, nomThick]

--derived inputs with units and uncertainties
derivedInsWUnitsUncrtn :: [UncertQ]
derivedInsWUnitsUncrtn = [standOffDist]

--derived inputs with uncertainties and no units
derivedInsWUncrtn :: [UncertQ]
derivedInsWUncrtn = [aspectRatio]

inputDataConstraints :: [UncertainChunk]
inputDataConstraints = map uncrtnw inputsWUnitsUncrtn ++
  map uncrtnw inputsWUncrtn

derivedInputDataConstraints :: [UncertainChunk]
derivedInputDataConstraints = map uncrtnw derivedInsWUnitsUncrtn
  ++ map uncrtnw derivedInsWUncrtn

dataConstraints :: [UncertainChunk]
dataConstraints = inputDataConstraints ++ derivedInputDataConstraints


plateLen = uqcND "plateLen" (nounPhraseSP "plate length (long dimension)")
  lA metre Real
  [ gtZeroConstr,
    physRange $ UpFrom (Inc, sy plateWidth),
    sfwrRange $ Bounded (Inc , sy dimMin) (Inc , sy dimMax)] (dbl 1.5) defaultUncrt

plateWidth = uqcND "plateWidth" (nounPhraseSP "plate width (short dimension)")
  lB metre Real
  [ physRange $ Bounded (Exc, exactDbl 0) (Inc, sy plateLen),
    sfwrRange $ Bounded (Inc, sy dimMin) (Inc, sy dimMax)] (dbl 1.2) defaultUncrt

aspectRatio = uq (constrained' (dqdNoUnit aspectRatioCon (variable "AR") Real)
  [ physRange $ UpFrom (Inc, exactDbl 1),
    sfwrRange $ UpTo (Inc, sy arMax)] (dbl 1.5)) defaultUncrt

pbTol = uvc "pbTol" (nounPhraseSP "tolerable probability of breakage")
  (sub cP (Concat [lBreak, lTol])) Real
  [probConstr] (dbl 0.008) (uncty 0.001 Nothing)

charWeight = uqcND "charWeight" (nounPhraseSP "charge weight")
  lW kilogram Real
  [ gtZeroConstr,
    sfwrRange $ Bounded (Inc, sy cWeightMin) (Inc, sy cWeightMax)]
    (exactDbl 42) defaultUncrt

tNT = uvc "tNT" (nounPhraseSP "TNT equivalent factor")
  (variable "TNT") Real
  [ gtZeroConstr ] (exactDbl 1) defaultUncrt

standOffDist = uq (constrained' (uc sD (variable "SD") Real metre)
  [ gtZeroConstr,
    sfwrRange $ Bounded (Inc, sy sdMin) (Inc, sy sdMax)] (exactDbl 45)) defaultUncrt

nomThick = cuc' "nomThick"
  (nounPhraseSent $ S "nominal thickness t is in" +:+ eS (mkSet Rational (map dbl nominalThicknesses)))
  "the specified standard thickness of the glass plate" lT millimetre
  {-Discrete nominalThicknesses, but not implemented-} Rational
  [sfwrElem $ mkSet Rational (map dbl nominalThicknesses)] $ exactDbl 8 -- for testing

glassTypeCon = constrainedNRV' (dqdNoUnit glassTy lG String)
  [sfwrElem $ mkSet String $ map (str . abrv . snd) glassType]


outputs :: [QuantityDict]
outputs = map qw [isSafePb, isSafeLR] ++ [qw probBr, qw stressDistFac]

-- | Symbols uniquely relevant to theory models.
tmSymbols :: [QuantityDict]
tmSymbols = map qw [probFail, pbTolfail]

probBr, probFail, pbTolfail, stressDistFac :: ConstrainedChunk
probBr = cvc "probBr" (nounPhraseSP "probability of breakage")
  (sub cP lBreak) Real
  [probConstr] (Just $ dbl 0.4)

stressDistFac = cvc "stressDistFac" (nounPhraseSP "stress distribution factor (Function)")
  cJ Real [physRange $ Bounded (Inc, sy stressDistFacMin) (Inc, sy stressDistFacMax)] (Just $ exactDbl 15)

probFail = cvc "probFail" (nounPhraseSP "probability of failure")
  (sub cP lFail) Real
  [probConstr] (Just $ dbl 0.4)

pbTolfail = cvc "pbTolfail" (nounPhraseSP "tolerable probability of failure")
  (sub cP (Concat [lFail, lTol])) Real
  [probConstr] (Just $ dbl 0.008)


  --FIXME: no typical value!

{--}

specParamVals :: [ConstQDef]
specParamVals = [dimMax, dimMin, arMax, cWeightMax, cWeightMin,
  sdMax, sdMin, stressDistFacMin, stressDistFacMax]

dimMax, dimMin, arMax, cWeightMax, cWeightMin, sdMax, stressDistFacMin, stressDistFacMax,
  sdMin :: ConstQDef

dimMax     = mkQuantDef (unitary "dimMax"
  (nounPhraseSP "maximum value for one of the dimensions of the glass plate")
  (subMax lD) metre Real) (exactDbl 5)

dimMin     = mkQuantDef (unitary "dimMin"
  (nounPhraseSP "minimum value for one of the dimensions of the glass plate")
  (subMin lD) metre Real) (dbl 0.1)

arMax     = mkQuantDef (vc "arMax"
  (nounPhraseSP "maximum aspect ratio")
  (subMax (variable "AR")) Real) (exactDbl 5)

cWeightMax = mkQuantDef (unitary "cWeightMax"
  (nounPhraseSP "maximum permissible input charge weight")
  (subMax (eqSymb charWeight)) kilogram Real) (exactDbl 910)

cWeightMin = mkQuantDef (unitary "cWeightMin"
  (nounPhraseSP "minimum permissible input charge weight")
  (subMin (eqSymb charWeight)) kilogram Real) (dbl 4.5)

sdMax     = mkQuantDef (unitary "sdMax"
  (nounPhraseSP "maximum stand off distance permissible for input")
  (subMax (eqSymb standOffDist)) metre Real) (exactDbl 130)

sdMin     = mkQuantDef (unitary "sdMin"
  (nounPhraseSP "minimum stand off distance permissible for input")
  (subMin (eqSymb standOffDist)) metre Real) (exactDbl 6)

stressDistFacMin = mkQuantDef (vc "stressDistFacMin" (nounPhraseSP "minimum value for the stress distribution factor")
  (subMin (eqSymb stressDistFac)) Real) (exactDbl 1)

stressDistFacMax = mkQuantDef (vc "stressDistFacMax" (nounPhraseSP "maximum value for the stress distribution factor")
  (subMax (eqSymb stressDistFac)) Real) (exactDbl 32)
{--}

unitalSymbols :: [UnitalChunk]
unitalSymbols = [demand, tmDemand, lRe, tmLRe, nonFactorL, eqTNTWeight,
  sflawParamK, sflawParamM, loadDur, minThick]

sdx, sdy, sdz :: UnitaryChunk

demand, tmDemand, lRe, tmLRe, minThick, nonFactorL, eqTNTWeight,
  sflawParamM, sflawParamK, loadDur, modElas :: UnitalChunk

demand      = uc demandq lQ Real pascal --correct Space used?

tmDemand    = uc load (variable "Load") Real pascal --correct Space used?

lRe         = uc loadResis (variable "LR") Real pascal --correct Space used?

tmLRe       = uc capacity (variable "capacity") Real pascal --correct Space used?

nonFactorL  = uc nonFactoredL (variable "NFL") Real pascal --correct Space used?

eqTNTWeight = uc eqTNTChar (sub (eqSymb charWeight) (eqSymb tNT)) Real
  kilogram

modElas     = uc modE cE Real pascal

minThick    = uc' "minThick" (nounPhraseSP "minimum thickness")
  (S "minimum thickness of the glass plate") lH Real metre

sflawParamK = uc' "sflawParamK" (nounPhraseSP "surface flaw parameter") --parameterize?
  (S ("surface flaw parameter related to the coefficient of " ++
    "variation of the glass strength data")) lK Real sFlawPU

sflawParamM = uc' "sflawParamM" (nounPhraseSP "surface flaw parameter") --parameterize?
  (S "surface flaw parameter related to the mean of the glass strength data")
  lM Real sFlawPU

loadDur     = uc' "loadDur"    (nounPhraseSP "duration of load")
  (S "the amount of time that a load is applied to the glass plate")
  (sub lT lDur) Real second

sdx         = unitary "sdx" (nounPhraseSent $ phrase standOffDist +:+ sParen (phrase xComp))
  (subX (eqSymb standOffDist)) metre Real

sdy         = unitary "sdy" (nounPhraseSent $ phrase standOffDist +:+ sParen (phrase yComp))
  (subY (eqSymb standOffDist)) metre Real

sdz         = unitary "sdz" (nounPhraseSent $ phrase standOffDist +:+ sParen (phrase zComp))
  (subZ (eqSymb standOffDist)) metre Real

{-Quantities-}

unitless :: [QuantityDict]
unitless = [riskFun, isSafePb, isSafeProb, isSafeLR, isSafeLoad,
  sdfTol, dimlessLoad, tolLoad] ++ map qw [gTF, loadSF, loadDF]

interps :: [QuantityDict]
interps = [interpY, interpZ]

riskFun, isSafePb, isSafeProb, isSafeLR, isSafeLoad, sdfTol,
  dimlessLoad, tolLoad, interpY, interpZ :: QuantityDict

gTF, loadSF, loadDF :: DefinedQuantityDict

dimlessLoad = vc "dimlessLoad" (nounPhraseSP "dimensionless load") (hat lQ) Real

gTF           = dqdNoUnit glTyFac (variable "GTF") Integer

isSafePb   = vc "isSafePb"   (nounPhraseSP "probability of glass breakage safety requirement")
  (variable "isSafePb")   Boolean
isSafeProb = vc "isSafeProb" (nounPhraseSP "probability of failure safety requirement")
  (variable "isSafeProb") Boolean
isSafeLR   = vc "isSafeLR"   (nounPhraseSP "3 second load equivalent resistance safety requirement")
  (variable "isSafeLR")   Boolean
isSafeLoad = vc "isSafeLoad" (nounPhraseSP "load resistance safety requirement")
  (variable "isSafeLoad") Boolean

interpY = vc "interpY" (nounPhraseSP "interpY") (variable "interpY") (mkFunction [String, Real, Real] Real)
interpZ = vc "interpZ" (nounPhraseSP "interpZ") (variable "interpZ") (mkFunction [String, Real, Real] Real)


loadDF        = dqdNoUnit loadDurFac (variable "LDF") Real
loadSF        = dqdNoUnit loadShareFac (variable "LSF") Real

riskFun = vc "riskFun" (nounPhraseSP "risk of failure") cB Real

sdfTol = vc "sdfTol" (nounPhraseSP "tolerable stress distribution factor")
  (sub (eqSymb stressDistFac) lTol) Real

tolLoad = vc "tolLoad" (nounPhraseSP "tolerable load")
  (sub (eqSymb dimlessLoad) lTol) Real



lBreak, lDur, lFail, lTol :: Symbol
lBreak = label "b"
lDur   = label "d"
lFail  = label "f"
lTol   = label "tol"

concepts :: [ConceptChunk]
concepts = [aspectRatioCon, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl,
  glTyFac, lateral, load, specDeLoad, loadResis, longDurLoad, nonFactoredL,
  glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar,
  sD, blast, blastTy, glassGeo, capacity, demandq, safeMessage, notSafe, bomb,
  explosion]

aspectRatioCon, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl,
  glTyFac, lateral, load, specDeLoad, loadDurFac, loadResis, longDurLoad, modE, nonFactoredL,
  glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar,
  sD, blast, blastTy, glassGeo, capacity, demandq, safeMessage, notSafe, bomb,
  explosion :: ConceptChunk

annealedGl    = cc' annealed
  (S "a flat, monolithic, glass lite which has uniform thickness where" +:+
  S "the residual surface stresses are almost zero, as defined in"+:+ refS astm2016)
aspectRatioCon   = cc aR
  ("the ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5")
blast         = dcc "blast"       (cn' "blast")
  "any kind of man-made explosion"
blastResisGla = dcc "blastResisGla"    (nounPhraseSP "blast resistant glazing")
  "glazing that provides protection against air blast pressure generated by explosions"
blastTy       = dcc "blastTy"     (cn' "blast type")
  ("the blast type input includes parameters like weight of charge, TNT " ++
    "equivalent factor, and stand off distance from the point of explosion")
bomb          = dcc "bomb"        (cn' "bomb") ("a container filled " ++
  "with a destructive substance designed to exlode on impact or via detonation")
capacity      = dcc "capacity"    (nounPhraseSP "capacity or load resistance")
  "load resistance calculated"
demandq       = dcc "demandq"     (nounPhraseSP "applied load (demand)")
  "3 second duration equivalent pressure"
eqTNTChar     = dcc "eqTNTChar"   (nounPhraseSP "equivalent TNT charge mass")
  "mass of TNT placed on the ground in a hemisphere that represents the design explosive threat"
explosion     = dcc "explosion"   (cn' "explosion")
  "a destructive shattering of something"
fTemperedGl   = cc' fullyT
  (foldlSent_ [S "a flat, monolithic, glass lite of uniform thickness that has",
  S "been subjected to a special heat treatment process where the residual",
  S "surface compression is not less than 69 MPa (10 000 psi) or the edge",
  S "compression not less than 67 MPa (9700 psi), as defined in", refS astm2012])
glassGeo      = dccWDS "glassGeo"    (cnIES "glass geometry")
  (S "the glass geometry based inputs include the dimensions of the" +:+
    foldlList Comma List [phrase glaPlane, phrase glassTy, phrase responseTy])
glassTy       = dcc "glassTy"     (cn' "glass type") "type of glass"
glassWL       = dcc "glassWL"     (nounPhraseSP "glass weight load")
  "the dead load component of the glass weight"
glBreakage    = dcc "glBreakage"  (nounPhraseSP "glass breakage")
  "the fracture or breakage of any lite or ply in monolithic, laminated, or insulating glass"
glTyFac       = cc' glassTypeFac
  (foldlSent_ [S "a multiplying factor for adjusting the", short lResistance,
  S "of different glass type, that is,", foldlList Comma Options glassTypeAbbrs
  `sC` S "in monolithic glass" `sC` short lGlass, sParen (titleize lGlass) `sC`
   S "or", short iGlass, sParen (titleize iGlass), S "constructions"])
hStrengthGl   = cc' heatS
  (foldlSent_ [S "a flat, monolithic, glass lite of uniform thickness that has",
  S "been subjected to a special heat treatment process where the residual",
  S "surface compression is not less than 24 MPa (3500psi) or greater than",
  S "52 MPa (7500 psi), as defined in", refS astm2012])
lateral       = dcc "lateral"     (nounPhraseSP "lateral")
  "perpendicular to the glass surface"
lite          = dcc "lite"        (cn' "lite")
  "pieces of glass that are cut, prepared, and used to create the window or door"
load          = dcc "load"        (nounPhraseSP "applied load (demand) or pressure")
  "a uniformly distributed lateral pressure"
loadDurFac    = cc' lDurFac (S "factor related to the effect of sustained loading on glass strength")
loadResis     = cc' lResistance
  (foldlSent_ [S "the uniform lateral load that a glass construction can sustain",
  S "based upon a given probability of breakage and load duration as defined in",
  complexRef astm2009 $ Page [1, 53]])
loadShareFac  = cc' lShareFac
  (foldlSent_ [S "a multiplying factor derived from the load sharing between the",
  S "double glazing, of equal or different thicknesses and types (including the",
  S "layered behaviour of", short lGlass, S "under long duration",
  S "loads), in a sealed", short iGlass, S "unit"])
longDurLoad   = dcc "longDurLoad"        (nounPhraseSP "long duration load")
  "any load lasting approximately 30 days"
modE = dcc "modElas" (nounPhraseSP "modulus of elasticity of glass")
  "the ratio of tensile stress to tensile strain of glass"
nonFactoredL  = cc' nFL
  (foldlSent_ [S "three second duration uniform load associated with a",
  S "probability of breakage less than or equal to 8", plural lite,
  S "per 1000 for monolithic", short annealed, S "glass"])
notSafe       = dcc "notSafe"     (nounPhraseSP "not safe")
  "For the given input parameters, the glass is NOT considered safe."
probBreak     = cc' probBr
  (foldlSent_ [S "the fraction of glass lites or plies that would break at the",
  S "first occurrence of a specified load and duration, typically expressed",
  S "in lites per 1000", sParen $ refS astm2016])
safeMessage   = dcc "safeMessage" (nounPhraseSP "safe")
  "For the given input parameters, the glass is considered safe."
sD            = cc' stdOffDist
  (S "the distance from the glazing surface to the centroid of a hemispherical" +:+
   S "high explosive charge")
shortDurLoad  = dcc "shortDurLoad"       (nounPhraseSP "short duration load")
  "any load lasting 3 seconds or less"
specA         = dcc "specA"       (nounPhraseSP "specifying authority")
  ("the design professional responsible for interpreting applicable " ++
    "regulations of authorities having jurisdiction and considering " ++
    "appropriate site specific factors to determine the appropriate " ++
    "values used to calculate the specified design load, and furnishing" ++
    " other information required to perform this practice")
specDeLoad    = dcc "specDeLoad"  (nounPhraseSP "specified design load")
  ("the magnitude in Pa (psf), type (for example, wind or snow) and " ++
    "duration of the load given by the specifying authority")

{--}

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

sdVector :: [UnitaryChunk]
sdVector = [sdx, sdy, sdz]

--
--Pulled to be used in "Terms And Definitions" Section--
termsWithDefsOnly, termsWithAccDefn, loadTypes, glassTypes :: [ConceptChunk]

glassTypes = [annealedGl, fTemperedGl, hStrengthGl]
termsWithDefsOnly = [glBreakage, lateral, lite, specA, blastResisGla, eqTNTChar]
termsWithAccDefn  = [sD, loadShareFac, glTyFac, aspectRatioCon]
loadTypes = [loadResis, nonFactoredL, glassWL, shortDurLoad, specDeLoad, longDurLoad]

--Defined for DataDefs.hs and this file only--
actualThicknesses :: [Double]
actualThicknesses = map snd glassThickness

nominalThicknesses :: [Double]
nominalThicknesses = map fst glassThickness

glassTypeFactors :: [Integer]
glassTypeFactors = map fst glassType

glassTypeAbbrs :: [Sentence]
glassTypeAbbrs = map (short . snd) glassType

type GlassType = [(Integer, CI)]         --[(Factor, Term)]
type GlassThickness = [(Double, Double)] --[(Nominal, Actual)]

glassType :: GlassType
glassType = [(1, annealed), (4, fullyT), (2, heatS)]

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
