module Drasil.GlassBR.Example where

import Drasil.GlassBR.Units

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Documentation
import Control.Lens((^.))
import Data.Char(toLower)
import Prelude hiding (log, id)

--FIXME: There are three separate non-factored loads! 
-- Two are (at least) linked, but the third is completely separate!

--FIXME: Clean up symbols (use symbol alphabet where possible)

glassBRSymbols :: [UnitalChunk]
glassBRSymbols = [plate_len, plate_width, dim_max, dim_min, mod_elas, 
  act_thick, sflawParamK, sflawParamM, demand, sd, sd_max, sd_min, nom_thick,
  load_dur, char_weight, cWeightMax, cWeightMin, eqTNTWeight]

plate_len, plate_width, dim_max, dim_min, mod_elas, act_thick, sflawParamK,
  sflawParamM, demand, sd, sdx, sdy, sdz, sd_max, sd_min, nom_thick, load_dur,
  char_weight, cWeightMax, cWeightMin, eqTNTWeight :: UnitalChunk

plate_len   = uc' "plate_len" "Plate length (long dimension)" 
  "FIXME: Define this or remove the need for definitions"
  lA millimetre
plate_width = uc' "plate_width" "Plate width (short dimension)" 
  "FIXME: Define this or remove the need for definitions"
  lB millimetre
dim_max     = uc' "dim_max" ("Maximum value for one of the dimensions of " ++
  "the glass plate") 
  "FIXME: Define this or remove the need for definitions"
  (sub lD (Atomic "max")) millimetre
dim_min     = uc' "dim_min" ("Minimum value for one of the dimensions of " ++
  "the glass plate") 
  "FIXME: Define this or remove the need for definitions"
  (sub lD (Atomic "min")) millimetre
mod_elas    = uc' "mod_elas" "Modulus of elasticity of glass" 
  "FIXME: Define this or remove the need for definitions"
  cE kilopascal
act_thick   = uc' "act_thick" "Actual thickness" 
  "FIXME: Define this or remove the need for definitions"
  lH millimetre
sflawParamK = uc' "sflawParamK" "Surface flaw parameter" 
  "FIXME: Define this or remove the need for definitions"
  lK sFlawPU
sflawParamM = uc' "sflawParamM" "Surface flaw parameter" 
  "FIXME: Define this or remove the need for definitions"
  lM sFlawPU
demand      = uc' "demand" "Applied load (demand)" 
  "FIXME: Define this or remove the need for definitions"
  lQ kilopascal
sd          = uc' "sd" ("Stand off distance which is represented in " ++
  "coordinates (SDx, SDy, SDz)") 
  "FIXME: Define this or remove the need for definitions"
  (Atomic "SD") metre
sdx         = uc' "sdx" "Stand off distance (x-component)" 
  "FIXME: Define this or remove the need for definitions"
  (sub (sd ^. symbol) lX) metre
sdy         = uc' "sdy" "Stand off distance (y-component)" 
  "FIXME: Define this or remove the need for definitions"
  (sub (sd ^. symbol) lY) metre
sdz         = uc' "sdz" "Stand off distance (z-component)" 
  "FIXME: Define this or remove the need for definitions"
  (sub (sd ^. symbol) lZ) metre
sd_max      = uc' "sd_max" ("Maximum stand off distance permissible " ++
  "for input") 
  "FIXME: Define this or remove the need for definitions"
  (sub (sd ^. symbol) (Atomic "max")) metre
sd_min      = uc' "sd_min" ("Minimum stand off distance permissible " ++
  "for input") 
  "FIXME: Define this or remove the need for definitions"
  (sub (sd ^. symbol) (Atomic "min")) metre
nom_thick   = uc' "nom_thick" ("Nominal thickness t in {2.5, 2.7, 3.0, 4.0, " ++
  "5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0}") 
  "FIXME: Define this or remove the need for definitions"
  lT millimetre
load_dur    = uc' "load_dur" "Duration of load" 
  "FIXME: Define this or remove the need for definitions"
  (sub lT lD) second
char_weight = uc' "char_weight" "Charge weight" 
  "FIXME: Define this or remove the need for definitions"
  lW kilogram
cWeightMax  = uc' "cWeightMax" "Maximum permissible input charge weight" 
  "FIXME: Define this or remove the need for definitions"
  (sub (char_weight ^. symbol) 
  (Atomic "max")) kilogram
cWeightMin  = uc' "cWeightMin" "Minimum permissible input charge weight" 
  "FIXME: Define this or remove the need for definitions"
  (sub (char_weight ^. symbol) (Atomic "min")) kilogram
eqTNTWeight = uc' "eqTNTWeight" "Explosive Mass in equivalent weight of TNT" 
  "FIXME: Define this or remove the need for definitions"
  (sub (char_weight ^. symbol) (tNT ^. symbol)) kilogram

glassBRUnitless :: [VarChunk]
glassBRUnitless = [risk_fun, glass_type, is_safe1, is_safe2, sdf, sdf_tol, 
  prob_br, pb_tol, dimlessLoad, tolLoad]

risk_fun, glass_type, is_safe1, is_safe2, sdf, sdf_tol, prob_br, pb_tol,
  dimlessLoad, tolLoad, tNT, lRe, loadSF, ar, ar_max, ar_min, gTF :: VarChunk

----Quantities--
risk_fun    = makeVC "risk_fun" "Risk function" cB
glass_type  = makeVC "glass_type" "Glass type, g in {AN, HS, FT}" lG
is_safe1    = makeVC "is_safe1" ("True when calculated probability is " ++
  "less than tolerable probability") (Concat [Atomic "is", Special UScore, 
  Atomic "safe1"])
is_safe2    = makeVC "is_safe2" ("True when load resistance (capacity) " ++
  "is greater than load (demand)") (Concat [Atomic "is", Special UScore, 
  Atomic "safe2"])
sdf         = makeVC "sdf" "Stress distribution factor (Function)" cJ
sdf_tol     = makeVC "sdf_tol" ("Stress distribution factor (Function) " ++
  "based on Pbtol") (sub (sdf ^. symbol) (Atomic "tol"))
prob_br     = makeVC "prob_br" "Probability of breakage" (sub cP lB)
pb_tol      = makeVC "pb_tol" "Tolerable probability of breakage" 
              (sub cP (Atomic "btol"))
dimlessLoad = makeVC "dimlessLoad" "Dimensionless load" (hat lQ)
tolLoad     = makeVC "tolLoad" "Tolerable load" 
  (sub (dimlessLoad ^. symbol) (Atomic "tol"))
tNT         = makeVC "tNT" "TNT equivalent factor" (Atomic "TNT")
lRe         = makeVC "lRe" "Load Resistance" (Atomic "LR")
loadSF      = makeVC "loadSF" "Load Share Factor" (Atomic "LSF")
ar          = makeVC "ar" "Aspect Ratio" (Atomic "AR")
ar_max      = makeVC "ar_max" "Maximum Aspect Ratio" 
  (sub (ar ^. symbol) (Atomic "max"))
ar_min      = makeVC "ar_min" "Minimum Aspect Ratio" 
  (sub (ar ^. symbol) (Atomic "min"))
gTF         = makeVC "gTF" "Glass Type Factor" (Atomic "GTF")

----Acronyms-----
-- FIXME: Use actual acronyms instead of CCs.
acronyms :: [CI]
acronyms = [assumption,annealedGlass,aspectR,aspectRMax,dataDefn,fullyTGlass,
  goalStmt,glassTypeFac,heatSGlass,iGlass,inModel,likelyChg,lDurFac,
  lGlass,lResistance,lShareFac,notApp,nonFactorL,physSyst,requirement,
  srs,thModel,eqTNT]
  
gLassBR :: ConceptChunk

annealedGlass, aspectR,aspectRMax,fullyTGlass,glassTypeFac,heatSGlass,
  iGlass,lDurFac, lGlass,lResistance,lShareFac,notApp,nonFactorL,
  eqTNT :: CI
--FIXME: So many of these are duplicates of other named chunks/concepts
  
gLassBR       = dcc "gLassBR" "GlassBR" "Glass-BR"
annealedGlass = commonidea "annealedGlass" "Annealed Glass" "AN"
aspectR       = commonidea "aspectR" "Aspect Ratio" "AR"
aspectRMax    = commonidea "aspectRMax" "Maximum Aspect Ratio" "ARmax"
fullyTGlass   = commonidea "fullyTGlass" "Fully Tempered Glass" "FT"
glassTypeFac  = commonidea "glassTypeFac" "Glass Type Factor" "GTF"
heatSGlass    = commonidea "heatSGlass" "Heat Strengthened Glass" "HS"
iGlass        = commonidea "iGlass" "Insulating Glass" "IG"
lDurFac       = commonidea "lDurFac" "Load Duration Factor" "LDF"
lGlass        = commonidea "lGlass" "Laminated Glass" "LG"
lResistance   = commonidea "lResistance" "Load Resistance" "LR"
lShareFac     = commonidea "lShareFac" "Load Share Factor" "LSF"
notApp        = commonidea "notApp" "Not Applicable" "N/A"
nonFactorL    = commonidea "nonFactorL" "Non-Factored Load" "NFL"
eqTNT         = commonidea "eqTNT" "TNT (Trinitrotoluene) Equivalent Factor" "TNT"


----Terminology---- 

-- TODO: See if we can make some of these terms less specific and/or
--    parameterized.
terms :: [ConceptChunk]
terms = [aR, gbr, lite, glassTy, an, ft, hs, gtf, lateral, load, specDeLoad, 
  lr, ldl, nfl, glassWL, sdl, lsf, pb, specA, blaReGLa, eqTNTChar, sD]

aR, gbr, lite, glassTy, an, ft, hs, gtf, lateral, load, specDeLoad, lr, 
  ldl, nfl, glassWL, sdl, lsf, pb, specA, blaReGLa, eqTNTChar, 
  sD, glaSlab, blast, blastTy, glassGeo, capacity, demandq, 
  safeMessage, notSafe:: ConceptChunk
  
aR            = dcc "aR" "Aspect ratio" 
  ("The ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5.")
gbr           = dcc "gbr" "Glass breakage" 
  ("The fracture or breakage of any lite or ply in monolithic, laminated, " ++
    "or insulating glass.")
lite          = dcc "lite" "Lite" 
  ("Pieces of glass that are cut, prepared, and used to create the window " ++
    "or door.")
glassTy       = dcc "glassTy" "Glass Types" "Type of glass"
an            = dcc "an" "Annealed glass"
  ("A flat, monolithic, glass lite which has uniform thickness where the " ++
    "residual surface stresses are almost zero, as defined in [5] in " ++
    "Reference.")
ft            = dcc "ft" "Fully tempered glass" 
  ("A flat and monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 69 MPa (10 000 psi) or the edge " ++
    "compression not less than 67 MPa (9700 psi), as defined in [6] in " ++
    "Reference.")
hs            = dcc "hs" "Heat strengthened glass" 
  ("A flat, monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 24 MPa (3500psi) or greater " ++
    "than 52 MPa (7500 psi), as defined in [6] in Reference.")
gtf           = dccWDS "gtf" "Glass type factor" 
  (S "A multiplying factor for adjusting the " :+: (getAcc lResistance) :+:
  S " of different glass type, that is, " :+: (getAcc annealedGlass) :+: 
  S ", " :+: (getAcc heatSGlass) :+: S ", or " :+: (getAcc fullyTGlass) :+: 
  S " in monolithic glass, " :+: (getAcc lGlass) :+: S " (Laminated Glass), " :+:
  S "or " :+: (getAcc iGlass) :+: S " (Insulating Glass) constructions.")
lateral       = dcc "lateral" "Lateral" "Perpendicular to the glass surface."
load          = dcc "load" "Load" "A uniformly distributed lateral pressure."
specDeLoad    = dcc "specDeLoad" "Specified design load" 
  ("The magnitude in kPa (psf), type (for example, wind or snow) and " ++
    "duration of the load given by the specifying authority.")
lr            = dcc "lr" "Load resistance" 
  ("The uniform lateral load that a glass construction can sustain based " ++
    "upon a given probability of breakage and load duration as defined in " ++
    "[4] in Reference.")
ldl           = dcc "ldl" "Long duration load" 
  ("Any load lasting approximately 30 days.")
nfl           = dccWDS "nfl" "Non-factored load"
  (S ("Three second duration uniform load associated with a probability of " ++
    "breakage less than or equal to 8 lites per 1000 for monolithic ") :+:
    (getAcc annealedGlass) :+: S " glass.")
glassWL       = dcc "glassWL" "Glass weight load" 
  ("The dead load component of the glass weight.")
sdl           = dcc "sdl" "Short duration load" "Any load lasting 3s or less."
lsf           = dccWDS "lsf" "Load share factor" 
  (S "A multiplying factor derived from the load sharing between the double " :+:
  S "glazing, of equal or different thickness's and types (including the " :+:
  S "layered behaviour of " :+: (getAcc lGlass) :+: S " under long duration " :+:
  S "loads), in a sealed " :+: (getAcc iGlass) :+: S " unit.")
pb            = dcc "pb" "Probability of breakage" 
  ("The fraction of glass lites or plies that would break at the first " ++
    "occurrence of a specified load and duration, typically expressed " ++
    "in lites per 1000.")
specA         = dcc "specA" "Specifying authority" 
  ("The design professional responsible for interpreting applicable " ++
    "regulations of authorities having jurisdiction and considering " ++
    "appropriate site specific factors to determine the appropriate " ++
    "values used to calculate the specified design load, and furnishing " ++
    "other information required to perform this practice.")
blaReGLa      = dcc "blaReGLa" "Blast resistant glazing" 
  ("Glazing that provides protection against air blast pressure generated " ++
    "by explosions.")
eqTNTChar     = dcc "eqTNTChar" "Equivalent TNT charge mass" 
  ("Mass of TNT placed on the ground in a hemisphere that represents the " ++
    "design explosive threat.")
sD            = dcc "sD" "Stand off distance" 
  ("The distance from the glazing surface to the centroid of a " ++
    "hemispherical high explosive charge.")
glaSlab       = dcc "glaSlab" "Glass slab" "Glass slab"
blast         = dcc "blast" "Blast" "Any kind of man-made explosion"
blastTy       = dcc "blastTy" "Blast type" 
  ("The blast type input includes parameters like weight of charge, TNT " ++
    "equivalent factor and stand off distance from the point of explosion.")
glassGeo      = dcc "glassGeo" "Glass geometry" 
  ("The glass geometry based inputs include the dimensions of the glass " ++
    "plane, glass type and response type.")
capacity      = dcc "capacity" "Capacity" "The load resistance calculated"
demandq       = dcc "demandq" "Demand" "3 second duration equivalent pressure"
safeMessage   = dcc "safeMessage" "Safe" 
  ("For the given input parameters, the glass is considered safe.")
notSafe       = dcc "notSafe" "Not safe" 
  ("For the given input parameters, the glass is NOT considered safe.")

--Theoretical models--
tModels :: [RelationConcept]
tModels = [t1SafetyReq, t2SafetyReq]

t1SafetyReq :: RelationConcept
t1SafetyReq = makeRC "t1SafetyReq" "Safety Requirement-1" 
  t1descr safety_require1_rel

safety_require1_rel :: Relation
safety_require1_rel = (C is_safe1) := (C prob_br) :< (C pb_tol)

--relation within relation
t1descr :: Sentence
t1descr = 
  S "If " :+: (P $ is_safe1 ^. symbol) :+: S " = True, the glass is " :+: 
  S "considered safe. " :+: (P $ is_safe1 ^.symbol) :+: S " and " :+: 
  (P $ is_safe2 ^. symbol) :+: S " (from " :+: 
  (makeRef (Definition (Theory t2SafetyReq))) :+: S ") are either " :+:
  S "both True or both False. " :+: (P $ prob_br ^. symbol) :+: 
  S " is the " :+: (sMap (map toLower) (prob_br ^. term)) :+: 
  S ", as calculated in " :+: (makeRef (Definition (Theory probOfBr))) :+: 
  S ". " :+: (P $ pb_tol ^. symbol) :+: S " is the " :+: 
  (sMap (map toLower) (pb_tol ^. term)) :+: S " entered by the user."

t2SafetyReq :: RelationConcept
t2SafetyReq = makeRC "t2SafetyReq" "Safety Requirement-2" 
  t2descr safety_require2_rel

safety_require2_rel :: Relation
safety_require2_rel = (C is_safe2) := (C lRe) :> (C demand)

--relation within relation

t2descr :: Sentence
t2descr = 
  S "If " :+: (P $ is_safe2 ^. symbol) :+: S " = True, the glass is " :+:
  S "considered safe. " :+: (P $ is_safe1 ^. symbol) :+: S " (from " :+:
  (makeRef (Definition (Theory t1SafetyReq))) :+: S " and " :+: 
  (P $ is_safe2 ^. symbol) :+: S " are either both True or both False. " :+:   
  (P $ lRe ^. symbol) :+: S " is the " :+: (lRe ^. term) :+: 
  S " (also called capacity, as defined in " :+: 
  (makeRef (Definition (Theory calOfCap))) :+: S ". " :+: 
  (P $ demand ^. symbol) :+: S " (also referred as the " :+: (demandq ^. term)
  :+: S ") is the " :+: (demandq ^. defn) :+: S ", as defined in " :+: 
  (makeRef (Definition (Theory calOfDe))) :+: S "."

--Instance Models--
iModels :: [RelationConcept]
iModels =[probOfBr, calOfCap, calOfDe]

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" "Probability of Glass Breakage" pbdescr pb_rel 

pb_rel :: Relation
pb_rel = (C prob_br) := 1 - (V "e") :^ (Neg (V "B"))

pbdescr :: Sentence
pbdescr =
  (P $ prob_br ^. symbol) :+: S " is the calculated " :+: 
    (sMap (map toLower) (prob_br ^. term)) :+: S ". "  :+: 
    (P $ risk_fun ^. symbol) :+: S " is the " :+: (risk ^. term) :+: S "."

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" "Calculation of Capacity(LR)" capdescr cap_rel

cap_rel :: Relation
cap_rel = (C lRe) := ((C nonFL):*(C glaTyFac):*(C loadSF)) 

capdescr :: Sentence
capdescr =
  (P $ lRe ^. symbol) :+: S " is the " :+: (lRe ^. term) :+: S ", which " :+:
  S "is also called capacity. " :+: (P $ nonFL ^. symbol) :+: S " is the " :+:
  (nonFL ^. term) :+: S ". " :+: (P $ gTF ^. symbol) :+: S " is the " :+:
  (gTF ^. term) :+: S ". " :+: (P $ loadSF ^. symbol) :+: S " is the " :+:
  (loadSF ^. term) :+: S "."

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" "Calculation of Demand(q)" dedescr de_rel

de_rel :: Relation
de_rel = (C demand) := FCall (C demand) [C eqTNTWeight, C sd] 

dedescr :: Sentence
dedescr = 
  (P $ demand ^. symbol) :+: S " or " :+: 
  (sMap (map toLower) (demandq ^. term)) :+: S ", is the " :+:
  (demandq ^. defn) :+: S " obtained from Figure 2 by interpolation using " 
  :+: (sMap (map toLower) (sD ^. term)) :+: S " (" :+: (P $ sd ^. symbol) 
  :+: S ") and " :+: (P $ eqTNTWeight ^. symbol) :+: S " as parameters. " :+: 
  (P $ eqTNTWeight ^. symbol) :+: S " is defined as " :+:
  (P $ eqTNTWeight ^. symbol) :+: S " = " :+: (P $ char_weight ^. symbol) :+:
  S " * TNT. " :+: (P $ char_weight ^. symbol) :+: S " is the " :+:
  (sMap (map toLower) (char_weight ^. term)) :+: S ". " :+: 
  (P $ tNT ^. symbol) :+: S " is the " :+: (tNT ^. term) :+: S ". " :+: 
  (P $ sd ^.symbol) :+: S " is the " :+: (sMap (map toLower) (sD ^. term))
  :+: S " where " :+: (P $ sd ^. symbol) :+: S " = ." 
  --equation in sentence

--Data Definitions--
dataDefns :: [QDefinition]
dataDefns = [risk,hFromt,loadDF,strDisFac,nonFL,glaTyFac,dL,tolPre,
  tolStrDisFac]

risk_eq :: Expr
risk_eq = ((C sflawParamK):/(Grouping (((C plate_len):/(Int 1000)):*
  ((C plate_width):/(Int 1000)))):^((C sflawParamM) - (Int 1))):*
  (Grouping ((Grouping ((C mod_elas):*(Int 1000))):*(Grouping ((C act_thick)
  :/(Int 1000))):^(Int 2))):^(C sflawParamM):*(C loadDF):*(V "e"):^(C sdf)

--FIXME? Maybe remove use of id, or not, since it's just setting an id.
--        Should definitely look into how this chunk is used to make sure
--        the current id makes sense. Same for the QDefns below.
  
risk :: QDefinition
risk = fromEqn' (risk_fun ^. id) (S "risk of failure") (risk_fun ^. symbol) 
  risk_eq

hFromt_eq :: Expr
hFromt_eq = FCall (C act_thick) [C nom_thick]

hFromt :: QDefinition
hFromt = fromEqn (act_thick ^. id) (S " function that maps from the " :+:
  S "nominal thickness (" :+: (P $ nom_thick ^. symbol) :+: S ") to the " :+:
  S "minimum thickness, as follows: h(t) = (t = 2.5 => 2.16 | t = 2.7 => " :+:
  S "2.59 | t = 3.0 => 2.92 | t = 4.0 => 3.78 | t = 5.0 => 4.57 | t = 6.0 " :+:
  S "=> 5.56 | t = 8.0 => 7.42 | t = 10.0 => 9.02 | t = 12.0 => 11.91 | " :+:
  S "t = 16.0 => 15.09 | t = 19.0 => 18.26 | t = 22.0 => 21.44)") 
  (act_thick ^.symbol) millimetre hFromt_eq

loadDF_eq :: Expr 
loadDF_eq = (Grouping ((C load_dur):/(Int 60))):^((C sflawParamM):/(Int 16))

--FIXME: Should we be using id here? My gut says no, but I'll look in 
-- more depth shortly.
-- Definitely should not have the id being printed (which it currently is)
loadDF :: QDefinition
loadDF = fromEqn' (lDurFac ^. id) (S "Load Duration Factor") (Atomic "LDF") 
  loadDF_eq

strDisFac_eq :: Expr
strDisFac_eq = FCall (C sdf) [C dimlessLoad, (C plate_len):/(C plate_width)]

strDisFac :: QDefinition
strDisFac = fromEqn' (sdf ^. id) (sdf ^. term) (sdf ^. symbol) 
  strDisFac_eq

nonFL_eq :: Expr
nonFL_eq = ((C tolLoad):*(C mod_elas):*(C act_thick):^(Int 4)):/
  ((Grouping ((C plate_len):*(C plate_width))):^(Int 2))

nonFL :: QDefinition
nonFL = fromEqn' (nonFactorL ^. id) (nonFactorL ^. term) (Atomic "NFL") 
  nonFL_eq

glaTyFac_eq :: Expr
glaTyFac_eq = FCall (C glaTyFac) [C glass_type]

glaTyFac :: QDefinition
glaTyFac = fromEqn' (glassTypeFac ^. id) (S "function that maps from " :+:
  S "the glass type (" :+: (P $ glass_type ^. symbol) :+: S ") to a real " :+:
  S "number, as follows: " :+: (P $ glaTyFac ^.symbol) :+: S "(" :+: 
  (P $ glass_type ^. symbol) :+: S ") = (" :+: (P $ glass_type ^. symbol) :+: 
  S " = AN => 1.0|" :+: (P $ glass_type ^. symbol) :+: S " = FT => 4.0|" :+: 
  (P $ glass_type ^. symbol) :+: S " = HS => 2.0). " :+: 
  (getAcc annealedGlass) :+: S " is " :+: 
  (sMap (map toLower) (annealedGlass ^. term)) :+: S ". " :+: 
  (getAcc fullyTGlass) :+: S " is " :+: 
  (sMap (map toLower) (fullyTGlass ^. term)) :+: S ". " :+:
  (getAcc heatSGlass) :+: S " is " :+: 
  (sMap (map toLower) (heatSGlass ^. term)) :+: S ".") (Atomic "GTF") 
  glaTyFac_eq

dL_eq :: Expr
dL_eq = ((C demand):*((Grouping ((C plate_len):*(C plate_width))):^(Int 2)))
  :/((C mod_elas):*((C act_thick):^(Int 4)):*(C gTF))

dL :: QDefinition
dL = fromEqn' (dimlessLoad ^. id) (dimlessLoad ^. term) 
  (dimlessLoad ^. symbol) dL_eq

tolPre_eq :: Expr
tolPre_eq = FCall (C tolLoad) [C sdf_tol, (C plate_len):/(C plate_width)]

tolPre :: QDefinition
tolPre = fromEqn' (tolLoad ^. id) (tolLoad ^. term) (tolLoad ^. symbol) 
  tolPre_eq

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = log (log ((Int 1):/((Int 1):-(C pb_tol)))
  :*((Grouping (((C plate_len):/(Int 1000)):*((C plate_width):/(Int 1000)))):^
  ((C sflawParamM) :- (Int 1)):/((C sflawParamK):*
  (Grouping (Grouping ((C mod_elas):*(Int 1000)):*
  (Grouping ((C act_thick):/(Int 1000))):^
  (Int 2))):^(C sflawParamM):*(C loadDF))))

tolStrDisFac :: QDefinition
tolStrDisFac = fromEqn' (sdf_tol ^. id) (sdf_tol ^. term) (sdf_tol ^. symbol) 
  tolStrDisFac_eq
