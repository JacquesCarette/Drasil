module Drasil.GlassBR.Unitals where

import Drasil.GlassBR.Units
import Drasil.GlassBR.Concepts

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Utils(symbolMapFun)
import Control.Lens((^.))
import Prelude hiding (log, id)
import Data.Drasil.SentenceStructures (foldlSent)

--FIXME: Many of the current terms can be separated into terms and defns!

{--}

glassBRSymbolsWithDefns :: [UnitalChunk]
glassBRSymbolsWithDefns = [mod_elas, standOffDist]

mod_elas, standOffDist :: UnitalChunk

mod_elas    = uc' "mod_elas"      (nounPhraseSP "modulus of elasticity of glass")
  "The ratio of tensile stress to tensile strain of glass." cE kilopascal
standOffDist = makeUCWDS "standOffDist"      (nounPhraseSP "stand off distance")
  (foldlSent [S "The distance from the glazing surface to the",
  S "centroid of a hemispherical high explosive charge. It is represented by", 
  S "the coordinates (SDx, SDy, SDz)"])
  (Atomic "SD") metre

{--}

glassBRSymbols :: [UnitaryChunk]
glassBRSymbols = [plate_len, plate_width, dim_max, dim_min, act_thick, sflawParamK,
  sflawParamM, demand, sdx, sdy, sdz, sd_max, sd_min, nom_thick, load_dur,
  char_weight, cWeightMax, cWeightMin, eqTNTWeight]

plate_len, plate_width, dim_max, dim_min, act_thick, sflawParamK,
  sflawParamM, demand, sdx, sdy, sdz, sd_max, sd_min, nom_thick, load_dur,
  char_weight, cWeightMax, cWeightMin, eqTNTWeight :: UnitaryChunk

plate_len   = unitary "plate_len"   (nounPhraseSP "plate length (long dimension)")
  lA millimetre Rational
plate_width = unitary "plate_width" (nounPhraseSP "plate width (short dimension)")
  lB millimetre Rational
dim_max     = unitary "dim_max"     (nounPhraseSP "maximum value for one of the dimensions of the glass plate") 
  (sub lD (Atomic "max")) millimetre Real
dim_min     = unitary "dim_min"     (nounPhraseSP "minimum value for one of the dimensions of the glass plate") 
  (sub lD (Atomic "min")) millimetre Real
act_thick   = unitary "act_thick"   (nounPhraseSP "actual thickness")
  lH millimetre Rational
sflawParamK = unitary "sflawParamK" (nounPhraseSP "surface flaw parameter") --parameterize?
  lK sFlawPU Rational
sflawParamM = unitary "sflawParamM" (nounPhraseSP "surface flaw parameter") --parameterize?
  lM sFlawPU Integer
demand      = unitary "demand"      (nounPhraseSP "applied load (demand)")
  lQ kilopascal Rational --correct Space used?
sdx         = unitary "sdx"         (nounPhraseSP "stand off distance (x-component)")
  (sub (standOffDist ^. symbol) lX) metre Rational
sdy         = unitary "sdy"         (nounPhraseSP "stand off distance (y-component)")
  (sub (standOffDist ^. symbol) lY) metre Rational
sdz         = unitary "sdz"         (nounPhraseSP "stand off distance (z-component)")
  (sub (standOffDist ^. symbol) lZ) metre Rational
sd_max      = unitary "sd_max"      (nounPhraseSP "maximum stand off distance permissible for input") 
  (sub (standOffDist ^. symbol) (Atomic "max")) metre Real
sd_min      = unitary "sd_min"      (nounPhraseSP "minimum stand off distance permissible for input") 
  (sub (standOffDist ^. symbol) (Atomic "min")) metre Real
nom_thick   = unitary "nom_thick"   (nounPhraseSP $ "nominal thickness t in {2.5, 2.7, 3.0, 4.0, " ++
  "5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0}") lT millimetre Rational
load_dur    = unitary "load_dur"    (nounPhraseSP "duration of load")
  (sub lT lD) second Integer
char_weight = unitary "char_weight" (nounPhraseSP "charge weight")
  lW kilogram Rational
cWeightMax  = unitary "cWeightMax"  (nounPhraseSP "maximum permissible input charge weight")
  (sub (char_weight ^. symbol) (Atomic "max")) kilogram Rational
cWeightMin  = unitary "cWeightMin"  (nounPhraseSP "minimum permissible input charge weight")
  (sub (char_weight ^. symbol) (Atomic "min")) kilogram Rational
eqTNTWeight = unitary "eqTNTWeight" (nounPhraseSP "explosive mass in equivalent weight of TNT") --replace with short TNT?
  (sub (char_weight ^. symbol) (tNT ^. symbol)) kilogram Rational

{-Quantities-}

glassBRUnitless :: [VarChunk]
glassBRUnitless = [ar_max, risk_fun, glass_type, is_safe1, is_safe2, stressDistFac, sdf_tol, prob_br,
  pb_tol, dimlessLoad, tolLoad, tNT, lRe, loadSF, ar_min, gTF]

ar_max, risk_fun, glass_type, is_safe1, is_safe2, stressDistFac, sdf_tol, prob_br,
  pb_tol, dimlessLoad, tolLoad, tNT, lRe, loadSF, ar_min, gTF :: VarChunk

ar_max      = vc "ar_max"        (nounPhraseSP "maximum aspect ratio")
  (sub (Atomic "AR") (Atomic "max")) Rational
risk_fun    = makeVC "risk_fun"      (nounPhraseSP "risk function") cB
glass_type  = vc "glass_type"    (nounPhraseSP "glass type, g in {AN, HS, FT}")
  lG String
is_safe1    = vc "is_safe1"      (nounPhraseSP $ "true when calculated probability is " ++
  "less than tolerable probability") (Concat [Atomic "is", Special UScore, 
  Atomic "safe1"]) Boolean
is_safe2    = vc "is_safe2"      (nounPhraseSP $ "true when load resistance (capacity) " ++
  "is greater than load (demand)") (Concat [Atomic "is", Special UScore, 
  Atomic "safe2"]) Boolean
stressDistFac = makeVC "stressDistFac"  (nounPhraseSP "stress distribution factor (Function)") cJ
sdf_tol     = makeVC "sdf_tol"       (nounPhraseSP "stress distribution factor (Function) based on Pbtol")
  (sub (stressDistFac ^. symbol) (Atomic "tol"))
prob_br     = vc "prob_br"       (nounPhraseSP "probability of breakage")
  (sub cP lB) Rational
pb_tol      = vc "pb_tol"        (nounPhraseSP "tolerable probability of breakage") (sub cP (Atomic "btol"))
  Rational
dimlessLoad = makeVC "dimlessLoad"   (nounPhraseSP "dimensionless load") (hat lQ)
tolLoad     = makeVC "tolLoad"       (nounPhraseSP "tolerable load")
  (sub (dimlessLoad ^. symbol) (Atomic "tol"))
tNT         = vc "tNT"           (nounPhraseSP "TNT equivalent factor") (Atomic "TNT") Rational
lRe         = makeVC "lRe"           (lResistance ^. term) (Atomic "LR")
loadSF      = vc "loadSF"        (lShareFac ^. term) (Atomic "LSF") Integer
ar_min      = vc "ar_min"        (nounPhraseSP "minimum aspect ratio")
  (sub (Atomic "AR") (Atomic "min")) Rational --find a way to call aspectR instead of using (Atomic "AR") again
gTF         = vc "gTF"           (glassTypeFac_ ^. term) (Atomic "GTF") Integer

terms :: [ConceptChunk]
terms = [aspectRatio, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl, glTyFac, lateral, load, specDeLoad, 
  loadResis, longDurLoad, nonFactoredL, glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar, sD]

aspectRatio, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl, glTyFac, lateral, load, specDeLoad, loadResis, 
  longDurLoad, nonFactoredL, glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar, 
  sD, blast, blastTy, glassGeo, capacity, demandq, safeMessage,
  notSafe, bomb, explosion :: ConceptChunk

--FIXME: Why are there multiple copies of aspect ratio, glass type factor, etc.?
aspectRatio   = dcc "aspectRatio" (aspectR ^. term)
  ("The ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5.")
glBreakage    = dcc "glBreakage"  (nounPhraseSP "glass breakage")
  ("The fracture or breakage of any lite or ply in monolithic, laminated, " ++
    "or insulating glass.")
lite          = dcc "lite"        (cn' "lite") --is used in the plural form
  ("Pieces of glass that are cut, prepared, and used to create the window " ++
    "or door.")
glassTy       = dcc "glassTy"     (cn' "glass types") "type of glass"
annealedGl    = dcc "annealedGl"  (annealedGlass ^. term)
  ("A flat, monolithic, glass lite which has uniform thickness where the " ++
    "residual surface stresses are almost zero, as defined in [5].")
fTemperedGl   = dcc "fTemperedGl"          (fullyTGlass ^. term)
  ("A flat and monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 69 MPa (10 000 psi) or the edge " ++
    "compression not less than 67 MPa (9700 psi), as defined in [6].")
hStrengthGl   = dcc "hStrengthGl"          (heatSGlass ^. term)
  ("A flat, monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 24 MPa (3500psi) or greater " ++
    "than 52 MPa (7500 psi), as defined in [6].")
glTyFac       = dccWDS "glTyFac"      (nounPhraseSP "glass type factor") 
  (foldlSent [S "A multiplying factor for adjusting the", (getAcc lResistance), 
  S "of different glass type, that is,", (getAcc annealedGlass) `sC` 
  (getAcc heatSGlass) `sC` S "or", (getAcc fullyTGlass), S "in monolithic glass" `sC`
  (getAcc lGlass), S "(Laminated Glass)" `sC` S "or", (getAcc iGlass), 
  S "(Insulating Glass) constructions"])
lateral       = dcc "lateral"     (nounPhraseSP "lateral") "Perpendicular to the glass surface."
load          = dcc "load"        (nounPhraseSP "load") "A uniformly distributed lateral pressure."
specDeLoad    = dcc "specDeLoad"  (nounPhraseSP "specified design load")
  ("The magnitude in kPa (psf), type (for example, wind or snow) and " ++
    "duration of the load given by the specifying authority.")
loadResis     = dcc "loadResis"          (lResistance ^. term)
  ("The uniform lateral load that a glass construction can sustain based " ++
    "upon a given probability of breakage and load duration as defined in " ++
    "[4 (pg. 1, 53)], following A2 and A1 respectively.")
longDurLoad   = dcc "longDurLoad"        (nounPhraseSP "long duration load")
  ("Any load lasting approximately 30 days.")
nonFactoredL  = dccWDS "nonFactoredL"    (nounPhraseSP "non-factored load")
  (foldlSent [S "Three second duration uniform load associated with a probability of",
    S "breakage less than or equal to 8", (plural lite), S "per 1000 for monolithic",
    (getAcc annealedGlass), S "glass"])
glassWL       = dcc "glassWL"     (nounPhraseSP "glass weight load")
  ("The dead load component of the glass weight.")
shortDurLoad  = dcc "shortDurLoad"       (nounPhraseSP "short duration load")
  "Any load lasting 3s or less."
loadShareFac  = dccWDS "loadShareFac"  (lShareFac ^. term)
  (foldlSent [S "A multiplying factor derived from the load sharing between the double",
  S "glazing, of equal or different thickness's and types (including the",
  S "layered behaviour of", (getAcc lGlass), S "under long duration",
  S "loads), in a sealed", (getAcc iGlass), S "unit"])
probBreak     = dcc "probBreak"       (prob_br ^. term)
  ("The fraction of glass lites or plies that would break at the first " ++
    "occurrence of a specified load and duration, typically expressed " ++
    "in lites per 1000.")
specA         = dcc "specA"       (nounPhraseSP "specifying authority")
  ("The design professional responsible for interpreting applicable " ++
    "regulations of authorities having jurisdiction and considering " ++
    "appropriate site specific factors to determine the appropriate " ++
    "values used to calculate the specified design load, and furnishing " ++
    "other information required to perform this practice.")
blastResisGla = dcc "blastResisGla"    (nounPhraseSP "blast resistant glazing")
  ("Glazing that provides protection against air blast pressure generated " ++
    "by explosions.")
eqTNTChar     = dcc "eqTNTChar"   (nounPhraseSP "equivalent TNT charge mass")
  ("Mass of TNT placed on the ground in a hemisphere that represents the " ++
    "design explosive threat.")
sD            = dccWDS "sD"       (standOffDist ^. term) (standOffDist ^. defn)
blast         = dcc "blast"       (nounPhraseSP "blast") "any kind of man-made explosion"
blastTy       = dcc "blastTy"     (nounPhraseSP "blast type")
  ("The blast type input includes parameters like weight of charge, TNT " ++
    "equivalent factor and stand off distance from the point of explosion.")
glassGeo      = dcc "glassGeo"    (nounPhraseSP "glass geometry")
  ("The glass geometry based inputs include the dimensions of the glass " ++
    "plane, glass type and response type.")
capacity      = dcc "capacity"    (nounPhraseSP "capacity") "the load resistance calculated"
demandq       = dcc "demandq"     (nounPhraseSP "demand") "3 second duration equivalent pressure"
safeMessage   = dcc "safeMessage" (nounPhraseSP "safe")
  ("For the given input parameters, the glass is considered safe.")
notSafe       = dcc "notSafe"     (nounPhraseSP "not safe")
  ("For the given input parameters, the glass is NOT considered safe.")
bomb          = dcc "bomb"        (nounPhraseSP "bomb") ("a container filled with a destructive" ++
  "substance designed to exlode on impact or via detonation")
explosion     = dcc "explosion"   (nounPhraseSP "explosion") "a destructive shattering of something"

-- hack; needs to be removed eventually
lDurFac :: VarChunk
lDurFac = makeVC "lDurFac" (nounPhraseSP "load duration factor") (Atomic "LDF")

temporary :: [ConVar]
temporary = [nonFactorL, glassTypeFac_]

nonFactorL, glassTypeFac_ :: ConVar
nonFactorL    = cvR (nonFactoredL) (Atomic "NFL")
glassTypeFac_  = cvR (glTyFac) (Atomic "GTF")

this_symbols :: [QSWrapper]
this_symbols = ((map qs glassBRSymbolsWithDefns) ++ (map qs glassBRSymbols)
  ++ (map qs glassBRUnitless))

temporaryLOSymbols :: [QSWrapper]
temporaryLOSymbols = this_symbols ++ map qs (temporary) ++ map qs [lDurFac]

gbSymbMap :: SymbolMap
gbSymbMap = symbolMap temporaryLOSymbols

gbSymbMapD :: QDefinition -> Contents
gbSymbMapD term_ = (symbolMapFun gbSymbMap Data) term_

gbSymbMapT :: RelationConcept -> Contents
gbSymbMapT term_ = (symbolMapFun gbSymbMap Theory) term_