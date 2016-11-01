module Drasil.GlassBR.Example where

import Drasil.GlassBR.Units

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Documentation

import Control.Lens((^.))
import Data.Char(toLower)
import Prelude hiding (log)

glassBRSymbols :: [UnitalChunk]
glassBRSymbols = [plate_len, plate_width, dim_max, dim_min, mod_elas, 
  act_thick, sflawParamK, sflawParamM, demand, sd, sd_max, sd_min, nom_thick,
  load_dur, char_weight, cWeightMax, cWeightMin, eqTNTWeight]

plate_len, plate_width, dim_max, dim_min, mod_elas, act_thick, sflawParamK,
  sflawParamM, demand, sd, sdx, sdy, sdz, sd_max, sd_min, nom_thick, load_dur,
  char_weight, cWeightMax, cWeightMin, eqTNTWeight :: UnitalChunk

plate_len   = makeUC "a" "Plate length (long dimension)" lA millimetre
plate_width = makeUC "b" "Plate width (short dimension)" lB millimetre
dim_max     = makeUC "d_max" ("Maximum value for one of the dimensions of " ++
  "the glass plate") (sub lD (Atomic "max")) millimetre
dim_min     = makeUC "d_min" ("Minimum value for one of the dimensions of " ++
  "the glass plate") (sub lD (Atomic "min")) millimetre
mod_elas    = makeUC "E" "Modulus of elasticity of glass" cE kilopascal
act_thick   = makeUC "h" "Actual thickness" lH millimetre
sflawParamK = makeUC "k" "Surface flaw parameter" lK sFlawPU
sflawParamM = makeUC "m" "Surface flaw parameter" lM sFlawPU
demand      = makeUC "q" "Applied load (demand)" lQ kilopascal
sd          = makeUC "SD" ("Stand off distance which is represented in " ++
  "coordinates (SDx, SDy, SDz)") (Atomic "SD") metre
sdx         = makeUC "SD_x" "Stand off distance (x-component)" 
  (sub (sd ^. symbol) lX) metre
sdy         = makeUC "SD_y" "Stand off distance (y-component)" 
  (sub (sd ^. symbol) lY) metre
sdz         = makeUC "SD_z" "Stand off distance (z-component)" 
  (sub (sd ^. symbol) lZ) metre
sd_max      = makeUC "SD_max" ("Maximum stand off distance permissible " ++
  "for input") (sub (sd ^. symbol) (Atomic "max")) metre
sd_min      = makeUC "SD_min" ("Minimum stand off distance permissible " ++
  "for input") (sub (sd ^. symbol) (Atomic "min")) metre
nom_thick   = makeUC "t" ("Nominal thickness t in {2.5, 2.7, 3.0, 4.0, " ++
  "5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0}") lT millimetre
load_dur    = makeUC "t_d" "Duration of load" (sub lT lD) second
char_weight = makeUC "w" "Charge weight" lW kilogram
cWeightMax  = makeUC "w_max" "Maximum permissible input charge weight" 
              (sub (char_weight ^. symbol) (Atomic "max")) kilogram
cWeightMin  = makeUC "w_min" "Minimum permissible input charge weight" 
              (sub (char_weight ^. symbol) (Atomic "min")) kilogram
eqTNTWeight = makeUC "w_TNT" "Explosive Mass in equivalent weight of TNT" 
              (sub (char_weight ^. symbol) (tNT ^. symbol)) kilogram

glassBRUnitless :: [VarChunk]
glassBRUnitless = [risk_fun, glass_type, is_safe1, is_safe2, sdf, sdf_tol, 
  prob_br, pb_tol, dimlessLoad, tolLoad]

risk_fun, glass_type, is_safe1, is_safe2, sdf, sdf_tol, prob_br, pb_tol,
  dimlessLoad, tolLoad, tNT, lRe, loadSF, ar, ar_max, ar_min, gTF :: VarChunk

----Quantities--
risk_fun    = makeVC "B" "Risk function" cB
glass_type  = makeVC "g" "Glass type, g in {AN, HS, FT}" lG
is_safe1    = makeVC "is_safe1" ("True when calculated probability is " ++
  "less than tolerable probability") (Concat [Atomic "is", Special UScore, 
  Atomic "safe1"])
is_safe2    = makeVC "is_safe2" ("True when load resistance (capacity) " ++
  "is greater than load (demand)") (Concat [Atomic "is", Special UScore, 
  Atomic "safe2"])
sdf         = makeVC "J" "Stress distribution factor (Function)" cJ
sdf_tol     = makeVC "J_tol" ("Stress distribution factor (Function) " ++
  "based on Pbtol") (sub (sdf ^. symbol) (Atomic "tol"))
prob_br     = makeVC "P_b" "Probability of breakage" (sub cP lB)
pb_tol      = makeVC "P_btol" "Tolerable probability of breakage" 
              (sub cP (Atomic "btol"))
dimlessLoad = makeVC "q_hat" "Dimensionless load" (hat lQ)
tolLoad     = makeVC "q_hat_tol" "Tolerable load" 
  (sub (dimlessLoad ^. symbol) (Atomic "tol"))
tNT         = makeVC "TNT" "TNT equivalent factor" (Atomic "TNT")
lRe         = makeVC "LR" "Load Resistance" (Atomic "LR")
loadSF      = makeVC "LSF" "Load Share Factor" (Atomic "LSF")
ar          = makeVC "AR" "Aspect Ratio" (Atomic "AR")
ar_max      = makeVC "AR_max" "Maximum Aspect Ratio" 
  (sub (ar ^. symbol) (Atomic "max"))
ar_min      = makeVC "AR_min" "Minimum Aspect Ratio" 
  (sub (ar ^. symbol) (Atomic "min"))
gTF         = makeVC "GTF" "Glass Type Factor" (Atomic "GTF")

----Acronyms-----
acronyms :: [ConceptChunk]
acronyms = [assumption,annealedGlass,aspectR,aspectRMax,dataDefn,fullyTGlass,
  goalStmt,glassTypeFac,heatSGlass,iGlass,inModel,likelyChg,lDurFac,
  lGlass,lResistance,lShareFac,notApp,nonFactorL,physSyst,requirement,
  srs,thModel,eqTNT]
  
gLassBR, annealedGlass, aspectR,aspectRMax,fullyTGlass,glassTypeFac,heatSGlass,
  iGlass,lDurFac, lGlass,lResistance,lShareFac,notApp,nonFactorL,
  eqTNT :: ConceptChunk
gLassBR       = makeCC "GlassBR" "Glass-BR"
annealedGlass = makeCC "AN" "Annealed Glass"
aspectR       = makeCC "AR" "Aspect Ratio"
aspectRMax    = makeCC "ARmax" "Maximum Aspect Ratio"
fullyTGlass   = makeCC "FT" "Fully Tempered Glass"
glassTypeFac  = makeCC "GTF" "Glass Type Factor"
heatSGlass    = makeCC "HS" "Heat Strengthened Glass"
iGlass        = makeCC "IG" "Insulating Glass"
lDurFac       = makeCC "LDF" "Load Duration Factor"
lGlass        = makeCC "LG" "Laminated Glass"
lResistance   = makeCC "LR" "Load Resistance"
lShareFac     = makeCC "LSF" "Load Share Factor"
notApp        = makeCC "N/A" "Not Applicable"
nonFactorL    = makeCC "NFL" "Non-Factored Load"
eqTNT         = makeCC "TNT" "TNT (Trinitrotoluene) Equivalent Factor"


----Terminology---- 
terms :: [ConceptChunk]
terms = [aR, gbr, lite, glassTy, an, ft, hs, gtf, lateral, load, specDeLoad, 
  lr, ldl, nfl, glassWL, sdl, lsf, pb, specA, blaReGLa, eqTNTChar, sD]

aR, gbr, lite, glassTy, an, ft, hs, gtf, lateral, load, specDeLoad, lr, 
  ldl, nfl, glassWL, sdl, lsf, pb, specA, blaReGLa, eqTNTChar, 
  sD, glaSlab, blast, blastTy, glassGeo, capacity, demandq, 
  safeMessage, notSafe:: ConceptChunk
  
aR            = makeCC "Aspect ratio" 
  ("The ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5.")
gbr           = makeCC "Glass breakage" 
  ("The fracture or breakage of any lite or ply in monolithic, laminated, " ++
    "or insulating glass.")
lite          = makeCC "Lite" 
  ("Pieces of glass that are cut, prepared, and used to create the window " ++
    "or door.")
glassTy       = makeCC "Glass Types" "Type of glass"
an            = makeCC "Annealed glass"
  ("A flat, monolithic, glass lite which has uniform thickness where the " ++
    "residual surface stresses are almost zero, as defined in [5] in " ++
    "Reference.")
ft            = makeCC "Fully tempered glass" 
  ("A flat and monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 69 MPa (10 000 psi) or the edge " ++
    "compression not less than 67 MPa (9700 psi), as defined in [6] in " ++
    "Reference.")
hs            = makeCC "Heat strengthened glass" 
  ("A flat, monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 24 MPa (3500psi) or greater " ++
    "than 52 MPa (7500 psi), as defined in [6] in Reference.")
gtf           = makeCC "Glass type factor" 
  ("A multiplying factor for adjusting the " ++ (lResistance ^. name) ++
    " of different glass type, that is, " ++ (annealedGlass ^. name) ++ 
    ", " ++ (heatSGlass ^. name) ++ ", or " ++ (fullyTGlass ^. name) ++ 
    " in monolithic glass, " ++ (lGlass ^. name) ++ " (Laminated Glass), " ++
    "or " ++ (iGlass ^. name) ++ " (Insulating Glass) constructions.")
lateral       = makeCC "Lateral" "Perpendicular to the glass surface."
load          = makeCC "Load" "A uniformly distributed lateral pressure."
specDeLoad    = makeCC "Specified design load" 
  ("The magnitude in kPa (psf), type (for example, wind or snow) and " ++
    "duration of the load given by the specifying authority.")
lr            = makeCC "Load resistance" 
  ("The uniform lateral load that a glass construction can sustain based " ++
    "upon a given probability of breakage and load duration as defined in " ++
    "[4] in Reference.")
ldl           = makeCC "Long duration load" 
  ("Any load lasting approximately 30 days.")
nfl           = makeCC "Non-factored load"
  ("Three second duration uniform load associated with a probability of " ++
    "breakage less than or equal to 8 lites per 1000 for monolithic " ++
    (annealedGlass ^. name) ++ " glass.")
glassWL       = makeCC "Glass weight load" 
  ("The dead load component of the glass weight.")
sdl           = makeCC "Short duration load" "Any load lasting 3s or less."
lsf           = makeCC "Load share factor" 
  ("A multiplying factor derived from the load sharing between the double " ++
    "glazing, of equal or different thickness's and types (including the " ++
    "layered behaviour of " ++ (lGlass ^. name) ++ " under long duration " ++
    "loads), in a sealed " ++ (iGlass ^. name) ++ " unit.")
pb            = makeCC "Probability of breakage" 
  ("The fraction of glass lites or plies that would break at the first " ++
    "occurrence of a specified load and duration, typically expressed " ++
    "in lites per 1000.")
specA         = makeCC "Specifying authority" 
  ("The design professional responsible for interpreting applicable " ++
    "regulations of authorities having jurisdiction and considering " ++
    "appropriate site specific factors to determine the appropriate " ++
    "values used to calculate the specified design load, and furnishing " ++
    "other information required to perform this practice.")
blaReGLa      = makeCC "Blast resistant glazing" 
  ("Glazing that provides protection against air blast pressure generated " ++
    "by explosions.")
eqTNTChar     = makeCC "Equivalent TNT charge mass" 
  ("Mass of TNT placed on the ground in a hemisphere that represents the " ++
    "design explosive threat.")
sD            = makeCC "Stand off distance" 
  ("The distance from the glazing surface to the centroid of a " ++
    "hemispherical high explosive charge.")
glaSlab       = makeCC "Glass slab" "Glass slab"
blast         = makeCC "Blast" "Any kind of man-made explosion"
blastTy       = makeCC "Blast type" 
  ("The blast type input includes parameters like weight of charge, TNT " ++
    "equivalent factor and stand off distance from the point of explosion.")
glassGeo      = makeCC "Glass geometry" 
  ("The glass geometry based inputs include the dimensions of the glass " ++
    "plane, glass type and response type.")
capacity      = makeCC "Capacity" "The load resistance calculated"
demandq       = makeCC "Demand" "3 second duration equivalent pressure"
safeMessage   = makeCC "Safe" 
  ("For the given input parameters, the glass is considered safe.")
notSafe       = makeCC "Not safe" 
  ("For the given input parameters, the glass is NOT considered safe.")

--Theoretical models--
tModels :: [RelationChunk]
tModels = [t1SafetyReq, t2SafetyReq]

t1SafetyReq :: RelationChunk
t1SafetyReq = makeRC "Safety Requirement-1" t1descr safety_require1_rel

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
  S " is the " :+: (sMap (map toLower) (prob_br ^. descr)) :+: 
  S ", as calculated in " :+: (makeRef (Definition (Theory probOfBr))) :+: 
  S ". " :+: (P $ pb_tol ^. symbol) :+: S " is the " :+: 
  (sMap (map toLower) (pb_tol ^. descr)) :+: S " entered by the user."

t2SafetyReq :: RelationChunk
t2SafetyReq = makeRC "Safety Requirement-2" t2descr safety_require2_rel

safety_require2_rel :: Relation
safety_require2_rel = (C is_safe2) := (C lRe) :> (C demand)

--relation within relation

t2descr :: Sentence
t2descr = 
  S "If " :+: (P $ is_safe2 ^. symbol) :+: S " = True, the glass is " :+:
  S "considered safe. " :+: (P $ is_safe1 ^. symbol) :+: S " (from " :+:
  (makeRef (Definition (Theory t1SafetyReq))) :+: S " and " :+: 
  (P $ is_safe2 ^. symbol) :+: S " are either both True or both False. " :+:   
  (P $ lRe ^. symbol) :+: S " is the " :+: (lRe ^. descr) :+: 
  S " (also called capacity, as defined in " :+: 
  (makeRef (Definition (Theory calOfCap))) :+: S ". " :+: 
  (P $ demand ^. symbol) :+: S (" (also referred as the " ++ (demandq ^. name)
  ++ ") is the ") :+: (demandq ^. descr) :+: S ", as defined in " :+: 
  (makeRef (Definition (Theory calOfDe))) :+: S "."

--Instance Models--
iModels :: [RelationChunk]
iModels =[probOfBr, calOfCap, calOfDe]

probOfBr :: RelationChunk
probOfBr = makeRC "Probability of Glass Breakage" pbdescr pb_rel 

pb_rel :: Relation
pb_rel = (C prob_br) := 1 - (V "e") :^ (Neg (V "B"))

pbdescr :: Sentence
pbdescr =
  (P $ prob_br ^. symbol) :+: S " is the calculated " :+: 
    (sMap (map toLower) (prob_br ^. descr)) :+: S ". "  :+: 
    (P $ risk_fun ^. symbol) :+: S " is the " :+: (risk ^. descr) :+: S "."

calOfCap :: RelationChunk
calOfCap = makeRC "Calculation of Capacity(LR)" capdescr cap_rel

cap_rel :: Relation
cap_rel = (C lRe) := ((C nonFL):*(C glaTyFac):*(C loadSF)) 

capdescr :: Sentence
capdescr =
  (P $ lRe ^. symbol) :+: S " is the " :+: (lRe ^. descr) :+: S ", which " :+:
  S "is also called capacity. " :+: (P $ nonFL ^. symbol) :+: S " is the " :+:
  (nonFL ^. descr) :+: S ". " :+: (P $ gTF ^. symbol) :+: S " is the " :+:
  (gTF ^. descr) :+: S ". " :+: (P $ loadSF ^. symbol) :+: S " is the " :+:
  (loadSF ^. descr) :+: S "."

calOfDe :: RelationChunk
calOfDe = makeRC "Calculation of Demand(q)" dedescr de_rel

de_rel :: Relation
de_rel = (C demand) := FCall (C demand) [C eqTNTWeight, C sd] 

dedescr :: Sentence
dedescr = 
  (P $ demand ^. symbol) :+: S " or " :+: 
  (sMap (map toLower) (S $ demandq ^. name)) :+: S ", is the " :+:
  (demandq ^. descr) :+: S " obtained from Figure 2 by interpolation using " 
  :+: (sMap (map toLower) (S $ sD ^. name)) :+: S " (" :+: (P $ sd ^. symbol) 
  :+: S ") and " :+: (P $ eqTNTWeight ^. symbol) :+: S " as parameters. " :+: 
  (P $ eqTNTWeight ^. symbol) :+: S " is defined as " :+:
  (P $ eqTNTWeight ^. symbol) :+: S " = " :+: (P $ char_weight ^. symbol) :+:
  S " * TNT. " :+: (P $ char_weight ^. symbol) :+: S " is the " :+:
  (sMap (map toLower) (char_weight ^. descr)) :+: S ". " :+: 
  (P $ tNT ^. symbol) :+: S " is the " :+: (tNT ^. descr) :+: S ". " :+: 
  (P $ sd ^.symbol) :+: S " is the " :+: (sMap (map toLower) (S $ sD ^. name))
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

risk :: QDefinition
risk = fromEqn' (risk_fun ^. name) (S "risk of failure") (risk_fun ^. symbol) 
  risk_eq

hFromt_eq :: Expr
hFromt_eq = FCall (C act_thick) [C nom_thick]

hFromt :: QDefinition
hFromt = fromEqn (act_thick ^. name) (S " function that maps from the " :+:
  S "nominal thickness (" :+: (P $ nom_thick ^. symbol) :+: S ") to the " :+:
  S "minimum thickness, as follows: h(t) = (t = 2.5 => 2.16 | t = 2.7 => " :+:
  S "2.59 | t = 3.0 => 2.92 | t = 4.0 => 3.78 | t = 5.0 => 4.57 | t = 6.0 " :+:
  S "=> 5.56 | t = 8.0 => 7.42 | t = 10.0 => 9.02 | t = 12.0 => 11.91 | " :+:
  S "t = 16.0 => 15.09 | t = 19.0 => 18.26 | t = 22.0 => 21.44)") 
  (act_thick ^.symbol) millimetre hFromt_eq

loadDF_eq :: Expr 
loadDF_eq = (Grouping ((C load_dur):/(Int 60))):^((C sflawParamM):/(Int 16))

loadDF :: QDefinition
loadDF = fromEqn' (lDurFac ^. name) (S "Load Duration Factor") (Atomic "LDF") 
  loadDF_eq

strDisFac_eq :: Expr
strDisFac_eq = FCall (C sdf) [C dimlessLoad, (C plate_len):/(C plate_width)]

strDisFac :: QDefinition
strDisFac = fromEqn' (sdf ^. name) (sdf ^. descr) (sdf ^. symbol) 
  strDisFac_eq

nonFL_eq :: Expr
nonFL_eq = ((C tolLoad):*(C mod_elas):*(C act_thick):^(Int 4)):/
  ((Grouping ((C plate_len):*(C plate_width))):^(Int 2))

nonFL :: QDefinition
nonFL = fromEqn' (nonFactorL ^. name) (nonFactorL ^. descr) (Atomic "NFL") 
  nonFL_eq

glaTyFac_eq :: Expr
glaTyFac_eq = FCall (C glaTyFac) [C glass_type]

glaTyFac :: QDefinition
glaTyFac = fromEqn' (glassTypeFac ^. name) (S "function that maps from " :+:
  S "the glass type (" :+: (P $ glass_type ^. symbol) :+: S ") to a real " :+:
  S "number, as follows: " :+: (P $ glaTyFac ^.symbol) :+: S "(" :+: 
  (P $ glass_type ^. symbol) :+: S ") = (" :+: (P $ glass_type ^. symbol) :+: 
  S " = AN => 1.0|" :+: (P $ glass_type ^. symbol) :+: S " = FT => 4.0|" :+: 
  (P $ glass_type ^. symbol) :+: S (" = HS => 2.0). " ++ 
  (annealedGlass ^. name) ++ " is ") :+: 
  (sMap (map toLower) (annealedGlass ^. descr)) :+: S (". " ++ 
  (fullyTGlass ^. name) ++ " is ") :+: 
  (sMap (map toLower) (fullyTGlass ^. descr)) :+: S (". " ++
  (heatSGlass ^. name) ++ " is ") :+: 
  (sMap (map toLower) (heatSGlass ^. descr)) :+: S ".") (Atomic "GTF") 
  glaTyFac_eq

dL_eq :: Expr
dL_eq = ((C demand):*((Grouping ((C plate_len):*(C plate_width))):^(Int 2)))
  :/((C mod_elas):*((C act_thick):^(Int 4)):*(C gTF))

dL :: QDefinition
dL = fromEqn' (dimlessLoad ^. name) (dimlessLoad ^. descr) 
  (dimlessLoad ^. symbol) dL_eq

tolPre_eq :: Expr
tolPre_eq = FCall (C tolLoad) [C sdf_tol, (C plate_len):/(C plate_width)]

tolPre :: QDefinition
tolPre = fromEqn' (tolLoad ^. name) (tolLoad ^. descr) (tolLoad ^. symbol) 
  tolPre_eq

tolStrDisFac_eq :: Expr
tolStrDisFac_eq = log (log ((Int 1):/((Int 1):-(C pb_tol)))
  :*((Grouping (((C plate_len):/(Int 1000)):*((C plate_width):/(Int 1000)))):^
  ((C sflawParamM) :- (Int 1)):/((C sflawParamK):*
  (Grouping (Grouping ((C mod_elas):*(Int 1000)):*
  (Grouping ((C act_thick):/(Int 1000))):^
  (Int 2))):^(C sflawParamM):*(C loadDF))))

tolStrDisFac :: QDefinition
tolStrDisFac = fromEqn' (sdf_tol ^. name) (sdf_tol ^. descr) (sdf_tol ^. symbol) 
  tolStrDisFac_eq
