module Drasil.GlassBR.Unitals where --whole file is used

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (mkQuantDef)
import Utils.Drasil

import Control.Lens ((^.))
import Prelude hiding (log)

import Data.Drasil.Concepts.Math (xComp, yComp, zComp)
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Quantities.Physics (subMax, subMin, subX, subY, subZ)
import Data.Drasil.SI_Units (kilogram, metre, millimetre, pascal, second)

import Drasil.GlassBR.Concepts (aR, annealed, fullyT, glaPlane, glassTypeFac, 
  heatS, iGlass, lGlass, lResistance, lShareFac, loadDurFactor, nFL, responseTy, 
  stdOffDist)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016)
import Drasil.GlassBR.Units (sFlawPU)

--FIXME: Many of the current terms can be separated into terms and defns?

{--}

symbolsWithDefns :: [UnitalChunk]
symbolsWithDefns = [modElas]

modElas :: UnitalChunk
modElas = uc' "modElas" (nounPhraseSP "modulus of elasticity of glass")
  "The ratio of tensile stress to tensile strain of glass." cE pascal

{--}

constrained :: [ConstrainedChunk]
constrained = map cnstrw inputDataConstraints ++ 
  [cnstrw probBr, cnstrw probFail] 

plateLen, plateWidth, charWeight, standOffDist :: UncertQ
aspectRatio, pbTol, tNT :: UncertainChunk
glassTypeCon, nomThick :: ConstrainedChunk

{--}

inputs :: [QuantityDict]
inputs = map qw inputsWUnitsUncrtn ++ map qw inputsWUncrtn ++ 
  map qw inputsNoUncrtn ++ map qw sdVector

--inputs with units and uncertainties
inputsWUnitsUncrtn :: [UncertQ]
inputsWUnitsUncrtn = [plateLen, plateWidth, charWeight]

--derived inputs with units and uncertainties
derivedInsWUnitsUncrtn :: [UncertQ]
derivedInsWUnitsUncrtn = [standOffDist]

--inputs with uncertainties and no units
inputsWUncrtn :: [UncertainChunk]
inputsWUncrtn = [pbTol, tNT]

--derived inputs with uncertainties and no units
derivedInsWUncrtn :: [UncertainChunk]
derivedInsWUncrtn = [aspectRatio]

--inputs with no uncertainties
inputsNoUncrtn :: [ConstrainedChunk]
inputsNoUncrtn = [glassTypeCon, nomThick]

inputDataConstraints :: [UncertainChunk]
inputDataConstraints = map uncrtnw inputsWUnitsUncrtn ++ 
  map uncrtnw inputsWUncrtn ++ map uncrtnw derivedInsWUnitsUncrtn ++ 
  map uncrtnw derivedInsWUncrtn

plateLen = uqcND "plateLen" (nounPhraseSP "plate length (long dimension)")
  lA metre Real 
  [ gtZeroConstr,
    physc $ UpFrom (Inc, sy plateWidth),
    sfwrc $ Bounded (Inc , sy dimMin) (Inc , sy dimMax)] (dbl 1.5) defaultUncrt

plateWidth = uqcND "plateWidth" (nounPhraseSP "plate width (short dimension)")
  lB metre Real
  [ physc $ Bounded (Exc, 0) (Inc, sy plateLen),
    sfwrc $ Bounded (Inc, sy dimMin) (Inc, sy dimMax)] (dbl 1.2) defaultUncrt

aspectRatio = uvc "aspectRatio" (aR ^. term)
  (Variable "AR") Real
  [ physc $ UpFrom (Inc, 1), 
    sfwrc $ UpTo (Inc, sy arMax)] (dbl 1.5) defaultUncrt

pbTol = uvc "pbTol" (nounPhraseSP "tolerable probability of breakage") 
  (sub cP (Concat [lBreak, lTol])) Real
  [ physc $ Bounded (Exc, 0) (Exc, 1)] (dbl 0.008) (uncty 0.001 Nothing)

charWeight = uqcND "charWeight" (nounPhraseSP "charge weight") 
  lW kilogram Real
  [ gtZeroConstr,
    sfwrc $ Bounded (Inc, sy cWeightMin) (Inc, sy cWeightMax)]
    (dbl 42) defaultUncrt

tNT = uvc "tNT" (nounPhraseSP "TNT equivalent factor")
  (Variable "TNT") Real
  [ gtZeroConstr ] (dbl 1.0) defaultUncrt

standOffDist = uqcND "standOffDist" (nounPhraseSP "stand off distance") 
  (Variable "SD") metre Real
  [ gtZeroConstr,
    sfwrc $ Bounded (Inc, sy sdMin) (Inc, sy sdMax)]
  (dbl 45) defaultUncrt
--FIXME: ^ incorporate definition in here?

nomThick = cuc "nomThick" 
  (nounPhraseSent $ S "nominal thickness" +:+ displayConstrntsAsSet 
    nomThick (map show nominalThicknesses))
  lT millimetre {-DiscreteD nominalThicknesses-} Rational 
  [enumc nominalThicknesses] 8

-- the S "glass type" is supposed to be using "phrase glassTy"
-- but the problem is still the Capitalization issue with new 
-- constructor `Ch` of generating the sentence. So for the sentence
-- only "S" can be capitalized 
glassTypeCon  = cvc "glassTypeCon" (nounPhraseSent $ S "glass type" +:+ 
    displayConstrntsAsSet glassTypeCon (map (getAccStr . snd) glassType))
  lG {-DiscreteS (map (getAccStr . snd) glassType)-} String
  [EnumeratedStr Software $ map (getAccStr . snd) glassType] Nothing

{--}

outputs :: [QuantityDict]
outputs = map qw [isSafePb, isSafeLR] ++ map qw [probBr]

tmSymbols :: [QuantityDict]
tmSymbols = map qw [probFail, pbTolfail] ++ map qw [isSafeProb, isSafeLoad] 

probBr, probFail, pbTolfail :: ConstrainedChunk
probBr = cvc "probBr" (nounPhraseSP "probability of breakage")
  (sub cP lBreak) Rational
  [ physc $ Bounded (Exc,0) (Exc,1)] (Just $ dbl 0.4)

probFail = cvc "probFail" (nounPhraseSP "probability of failure")
  (sub cP lFail) Rational
  [ physc $ Bounded (Exc,0) (Exc,1)] (Just $ dbl 0.4)

pbTolfail = cvc "pbTolfail" (nounPhraseSP "tolerable probability of failure") 
  (sub cP (Concat [lFail, lTol])) Real
  [ physc $ Bounded (Exc, 0) (Exc, 1)] (Just $ dbl 0.008) 
  

  --FIXME: no typical value!

{--}

specParamVals :: [QDefinition]
specParamVals = [dimMax, dimMin, arMax, cWeightMax, cWeightMin,
  sdMax, sdMin]

dimMax, dimMin, arMax, cWeightMax, cWeightMin, sdMax,
  sdMin :: QDefinition

dimMax     = mkQuantDef (unitary "dimMax"
  (nounPhraseSP "maximum value for one of the dimensions of the glass plate") 
  (subMax lD) metre Real) (dbl 5)

dimMin     = mkQuantDef (unitary "dimMin"
  (nounPhraseSP "minimum value for one of the dimensions of the glass plate") 
  (subMin lD) metre Real) (dbl 0.1)

arMax     = mkQuantDef (vc "arMax"
  (nounPhraseSP "maximum aspect ratio")
  (subMax (Variable "AR")) Rational) (dbl 5)

cWeightMax = mkQuantDef (unitary "cWeightMax" 
  (nounPhraseSP "maximum permissible input charge weight")
  (subMax (eqSymb charWeight)) kilogram Rational) (dbl 910)

cWeightMin = mkQuantDef (unitary "cWeightMin"
  (nounPhraseSP "minimum permissible input charge weight")
  (subMin (eqSymb charWeight)) kilogram Rational) (dbl 4.5)

sdMax     = mkQuantDef (unitary "sdMax"
  (nounPhraseSP "maximum stand off distance permissible for input")
  (subMax (eqSymb standOffDist)) metre Real) (dbl 130)

sdMin     = mkQuantDef (unitary "sdMin"
  (nounPhraseSP "minimum stand off distance permissible for input") 
  (subMin (eqSymb standOffDist)) metre Real) (dbl 6)


{--}

symbols :: [UnitaryChunk]
symbols = [minThick, sflawParamK, sflawParamM, demand, tmDemand, lRe, tmLRe, nonFactorL, loadDur,
  eqTNTWeight]

minThick, sflawParamK, sflawParamM, demand, tmDemand, sdx, sdy, sdz, lRe, tmLRe, nonFactorL, loadDur,
  eqTNTWeight :: UnitaryChunk

demand      = unitary "demand"      (nounPhraseSP "applied load (demand)")
  lQ pascal Rational --correct Space used?

tmDemand      = unitary "tmDemand"      (nounPhraseSP "applied load (demand) or pressure")
  (Variable "Load") pascal Rational --correct Space used?
  
lRe      = unitary "lRe"      (nounPhraseSP "load resistance")
  (Variable "LR") pascal Rational --correct Space used?

tmLRe      = unitary "tmLRe"      (nounPhraseSP "capacity or load resistance")
  (Variable "capacity") pascal Rational --correct Space used?

nonFactorL      = unitary "nonFactorL"      (nounPhraseSP "non-factored load")
  (Variable "NFL") pascal Rational --correct Space used?


eqTNTWeight = unitary "eqTNTWeight" 
  (nounPhraseSP "explosive mass in equivalent weight of TNT")
  (sub (eqSymb charWeight) (eqSymb tNT)) kilogram Real

loadDur    = unitary "loadDur"    (nounPhraseSP "duration of load")
  (sub lT lDur) second Real

minThick   = unitary "minThick"   (nounPhraseSP "minimum thickness")
  lH metre Rational

sdx         = unitary "sdx" (nounPhraseSent $ phrase standOffDist +:+ sParen (phrase xComp))
  (subX (eqSymb standOffDist)) metre Real

sdy         = unitary "sdy" (nounPhraseSent $ phrase standOffDist +:+ sParen (phrase yComp))
  (subY (eqSymb standOffDist)) metre Real

sdz         = unitary "sdz" (nounPhraseSent $ phrase standOffDist +:+ sParen (phrase zComp))
  (subZ (eqSymb standOffDist)) metre Real

sflawParamK = unitary "sflawParamK" (nounPhraseSP "surface flaw parameter") --parameterize?
  lK sFlawPU Real

sflawParamM = unitary "sflawParamM" (nounPhraseSP "surface flaw parameter") --parameterize?
  lM sFlawPU Real

{-Quantities-}

unitless :: [QuantityDict]
unitless = [riskFun, isSafePb, isSafeProb, isSafeLR, isSafeLoad, stressDistFac, sdfTol,
  dimlessLoad, tolLoad, loadSF, gTF, lDurFac]

riskFun, isSafePb, isSafeProb, isSafeLR, isSafeLoad, stressDistFac, sdfTol,
  dimlessLoad, tolLoad, loadSF, gTF, lDurFac :: QuantityDict


dimlessLoad   = vc "dimlessLoad" (nounPhraseSP "dimensionless load")
  (hat lQ) Real

gTF           = vc "gTF"             (glassTypeFac ^. term) (Variable "GTF") Integer

isSafePb      = vc "isSafePb"        (nounPhraseSP $ "variable that is assigned true when calculated" ++
  " probability is less than tolerable probability")
  (Variable "is-safePb") Boolean

isSafeProb      = vc "isSafeProb"        (nounPhraseSP $ "variable that is assigned true when" ++
  " probability of failure is less than tolerable probability of failure")
  (Variable "is-safeProb") Boolean

isSafeLR      = vc "isSafeLR"        (nounPhraseSP $ "variable that is assigned true when load resistance"
  ++ " (capacity) is greater than load (demand)")
  (Variable "is-safeLR") Boolean

isSafeLoad      = vc "isSafeLoad"        (nounPhraseSP $ "variable that is assigned true when load resistance"
  ++ " (capacity) is greater than applied load (demand)")
  (Variable "is-safeLoad") Boolean

lDurFac       = vc'' loadDurFactor (Variable "LDF") Real

loadSF        = vc'' lShareFac (Variable "LSF") Natural


riskFun      = vc "riskFun"    (nounPhraseSP "risk of failure") cB Real

sdfTol       = vc "sdfTol"     (nounPhraseSP $ "stress distribution" ++
  " factor (Function) based on Pbtol") 
  (sub (eqSymb stressDistFac) lTol) Real

stressDistFac = vc "stressDistFac" (nounPhraseSP $ "stress distribution" 
  ++ " factor (Function)") cJ Real

tolLoad       = vc "tolLoad"       (nounPhraseSP "tolerable load")
  (sub (eqSymb dimlessLoad) lTol) Real

lBreak, lDur, lFail, lTol :: Symbol
lBreak = Label "b"
lDur   = Label "d"
lFail  = Label "f"
lTol   = Label "tol"

terms :: [ConceptChunk]
terms = [aspectRatioCon, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl,
  glTyFac, lateral, load, specDeLoad, loadResis, longDurLoad, nonFactoredL,
  glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar,
  sD, blast, blastTy, glassGeo, capacity, demandq, safeMessage, notSafe, bomb,
  explosion]

aspectRatioCon, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl,
  glTyFac, lateral, load, specDeLoad, loadResis, longDurLoad, nonFactoredL,
  glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar,
  sD, blast, blastTy, glassGeo, capacity, demandq, safeMessage, notSafe, bomb,
  explosion :: ConceptChunk

annealedGl    = cc' annealed
  (foldlSent [S "A flat, monolithic, glass lite which has uniform thickness where",
  S "the residual surface stresses are almost zero, as defined in", makeCiteS astm2016])
aspectRatioCon   = cc aR
  ("The ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5.")
blast         = dcc "blast"       (nounPhraseSP "blast")
  "any kind of man-made explosion"
blastResisGla = dcc "blastResisGla"    (nounPhraseSP "blast resistant glazing")
  ("Glazing that provides protection against air blast pressure generated " ++
    "by explosions.")
blastTy       = dcc "blastTy"     (nounPhraseSP "blast type")
  ("The blast type input includes parameters like weight of charge, TNT " ++
    "equivalent factor, and stand off distance from the point of explosion.")
bomb          = dcc "bomb"        (nounPhraseSP "bomb") ("a container filled " ++
  "with a destructive substance designed to exlode on impact or via detonation")
capacity      = dcc "capacity"    (nounPhraseSP "capacity")
  "load resistance calculated"
demandq       = dcc "demandq"     (nounPhraseSP "demand") 
  "3 second duration equivalent pressure"
eqTNTChar     = dcc "eqTNTChar"   (nounPhraseSP "equivalent TNT charge mass")
  ("Mass of TNT placed on the ground in a hemisphere that represents the " ++
    "design explosive threat.")
explosion     = dcc "explosion"   (nounPhraseSP "explosion") 
  "a destructive shattering of something"
fTemperedGl   = cc' fullyT
  (foldlSent [S "A flat, monolithic, glass lite of uniform thickness that has",
  S "been subjected to a special heat treatment process where the residual",
  S "surface compression is not less than 69 MPa (10 000 psi) or the edge",
  S "compression not less than 67 MPa (9700 psi), as defined in", makeCiteS astm2012])
glassGeo      = dccWDS "glassGeo"    (nounPhraseSP "glass geometry")
  (S "The glass geometry based inputs include the dimensions of the" +:+ 
    phrase glaPlane `sC` phrase glassTy `sC` S "and" +:+.  phrase responseTy)
glassTy       = dcc "glassTy"     (cn' "glass type") "type of glass"
glassWL       = dcc "glassWL"     (nounPhraseSP "glass weight load")
  "The dead load component of the glass weight."
glBreakage    = dcc "glBreakage"  (nounPhraseSP "glass breakage")
  ("The fracture or breakage of any lite or ply in monolithic, laminated, " ++
    "or insulating glass.")
glTyFac       = cc' glassTypeFac
  (foldlSent [S "A multiplying factor for adjusting the", getAcc lResistance, 
  S "of different glass type, that is,", foldlList Comma Options glassTypeAbbrs
  `sC` S "in monolithic glass" `sC` getAcc lGlass, sParen (titleize lGlass) `sC`
   S "or", getAcc iGlass, sParen (titleize iGlass), S "constructions"])
hStrengthGl   = cc' heatS
  (foldlSent [S "A flat, monolithic, glass lite of uniform thickness that has",
  S "been subjected to a special heat treatment process where the residual",
  S "surface compression is not less than 24 MPa (3500psi) or greater than",
  S "52 MPa (7500 psi), as defined in", makeCiteS astm2012])
lateral       = dcc "lateral"     (nounPhraseSP "lateral") 
  "Perpendicular to the glass surface."
lite          = dcc "lite"        (cn' "lite")
  ("Pieces of glass that are cut, prepared, and used to create the window " ++
    "or door.")
load          = dcc "load"        (nounPhraseSP "load") 
  "A uniformly distributed lateral pressure."
loadResis     = cc' lResistance
  (foldlSent [S "The uniform lateral load that a glass construction can sustain",
  S "based upon a given probability of breakage and load duration as defined in",
  makeCiteInfoS astm2009 $ Page [1, 53]])
loadShareFac  = cc' lShareFac
  (foldlSent [S "A multiplying factor derived from the load sharing between the",
  S "double glazing, of equal or different thicknesses and types (including the",
  S "layered behaviour of", getAcc lGlass, S "under long duration",
  S "loads), in a sealed", getAcc iGlass, S "unit"])
longDurLoad   = dcc "longDurLoad"        (nounPhraseSP "long duration load")
  "Any load lasting approximately 30 days."
nonFactoredL  = cc' nFL
  (foldlSent [S "Three second duration uniform load associated with a", 
  S "probability of breakage less than or equal to 8", plural lite,
  S "per 1000 for monolithic", getAcc annealed, S "glass"])
notSafe       = dcc "notSafe"     (nounPhraseSP "not safe")
  "For the given input parameters, the glass is NOT considered safe."
probBreak     = cc' probBr
  (foldlSent [S "The fraction of glass lites or plies that would break at the",
  S "first occurrence of a specified load and duration, typically expressed",
  S "in lites per 1000", sParen $ makeCiteS astm2016])
safeMessage   = dcc "safeMessage" (nounPhraseSP "safe")
  "For the given input parameters, the glass is considered safe."
sD            = cc' stdOffDist
  (S "The distance from the glazing surface to the centroid of a hemispherical"
   +:+ S "high explosive charge. It is represented by the coordinates" +:+.
   sParen sdVectorSent)
shortDurLoad  = dcc "shortDurLoad"       (nounPhraseSP "short duration load")
  "Any load lasting 3 seconds or less."
specA         = dcc "specA"       (nounPhraseSP "specifying authority")
  ("The design professional responsible for interpreting applicable " ++
    "regulations of authorities having jurisdiction and considering " ++
    "appropriate site specific factors to determine the appropriate " ++
    "values used to calculate the specified design load, and furnishing" ++
    " other information required to perform this practice.")
specDeLoad    = dcc "specDeLoad"  (nounPhraseSP "specified design load")
  ("The magnitude in Pa (psf), type (for example, wind or snow) and " ++
    "duration of the load given by the specifying authority.")

{--}


{--}

--Constants--

constants :: [QDefinition]
constants = [constantM, constantK, constantModElas, constantLoadDur, constantLoadSF]
                ++ specParamVals 

constantM, constantK, constantModElas, constantLoadDur, constantLoadSF :: QDefinition
constantK       = mkQuantDef sflawParamK $ dbl 2.86e-53
constantM       = mkQuantDef sflawParamM $ dbl 7
constantModElas = mkQuantDef modElas     $ dbl 7.17e10
constantLoadDur = mkQuantDef loadDur     $ dbl 3
constantLoadSF  = mkQuantDef loadSF      1
--Equations--

sdVectorSent :: Sentence
sdVectorSent = foldlsC (map ch sdVector)

sdVector :: [UnitaryChunk]
sdVector = [sdx, sdy, sdz]

--
--Pulled to be used in "Terms And Definitions" Section--
termsWithDefsOnly, termsWithAccDefn, loadTypes, glassTypes :: [ConceptChunk]

glassTypes = [annealedGl, fTemperedGl, hStrengthGl]
termsWithDefsOnly = [glBreakage, lateral, lite, specA, blastResisGla,
  eqTNTChar]
termsWithAccDefn  = [sD, loadShareFac, glTyFac, aspectRatioCon]
loadTypes = [loadResis, nonFactoredL, glassWL, shortDurLoad,
  specDeLoad, longDurLoad] 

--Defined for DataDefs.hs and this file only--
actualThicknesses :: [Double]
actualThicknesses = map snd glassThickness

nominalThicknesses :: [Double]
nominalThicknesses = map fst glassThickness 

glassTypeFactors :: [Integer]
glassTypeFactors = map fst glassType

glassTypeAbbrs :: [Sentence]
glassTypeAbbrs = map (getAcc . snd) glassType

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
lateralLoad :: NamedChunk
lateralLoad  = compoundNC lateral load
