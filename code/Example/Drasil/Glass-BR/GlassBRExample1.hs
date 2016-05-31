{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module GlassBRExample1 where
import Language.Drasil.Expr (Expr(..), Relation(..))
import Language.Drasil.SI_Units2
import Language.Drasil.Symbol
import Language.Drasil.Chunk.Unital
import GlassBRUnits1
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Chunk (ConceptChunk(..),VarChunk(..),symbol,makeCC,makeVC)
import Language.Drasil.Spec (Sentence(..))
import Control.Lens((^.))
import Language.Drasil.Chunk.Relation
import Language.Drasil.Instances ()

glassBRSymbols :: [UnitalChunk]
glassBRSymbols = [plate_len, risk_fun, plate_width, dim_max, dim_min, mod_elas,
  glass_type, act_thick, is_safe1, is_safe2, sdf, sdf_tol, sflawParamK, 
  sflawParamM, prob_br, pb_tol, demand, dimlessLoad, tolLoad,sd, sd_max,
  sd_min, nom_thick, load_dur, char_weight, cWeightMax, cWeightMin, eqTNTWeight]

plate_len, risk_fun, plate_width, dim_max, dim_min, 
  mod_elas,glass_type, act_thick, is_safe1, is_safe2,
  sdf, sdf_tol, sflawParamK, sflawParamM, prob_br, pb_tol, 
  demand, dimlessLoad, tolLoad, sd, sd_max, sd_min, nom_thick, 
  load_dur, char_weight, cWeightMax, cWeightMin, eqTNTWeight:: UnitalChunk

plate_len   = makeUC "a" "Plate length (long dimension)" lA millimetre
plate_width = makeUC "b" "Plate width" lB millimetre
dim_max     = makeUC "d_max" "Maximum value for one of the dimensions of the glass plate"
              (sub lD (Atomic "max")) millimetre
dim_min     = makeUC "d_min" "Minimum value for one of the dimensions of the glass plate" 
              (sub lD (Atomic "min")) millimetre
mod_elas    = makeUC "E" "Modulus of elasticity of glass" cE kilopascal
act_thick   = makeUC "h" "Actual thickness" lH millimetre
sflawParamK = makeUC "k" "Surface flaw parameter" lK sFlawPU
sflawParamM = makeUC "m" "Surface flaw parameter" lM sFlawPU
demand      = makeUC "q" "Applied load (demand)" lQ kilopascal
sd          = makeUC "SD" "Stand off distance which is represented in coordinates (SDx, SDy, SDz)"
              (Concat [cS,cD]) metre
sd_max      = makeUC "SD_max" "Maximum stand off distance permissible for input" 
              (sub (Concat [cS,cD]) (Atomic "max")) metre
sd_min      = makeUC "SD_min" "Minimum stand off distance permissible for input" 
              (sub (Concat [cS,cD]) (Atomic "min")) metre
nom_thick   = makeUC "t" "Nominal thickness t in (2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0" 
              lT millimetre
load_dur    = makeUC "t_d" "Duration of load" (sub lT lD) second
char_weight = makeUC "w" "Charge weight" lW kilogram
cWeightMax  = makeUC "w_max" "Maximum permissible input charge weight" 
              (sub lW (Atomic "max")) kilogram
cWeightMin  = makeUC "w_min" "Minimum permissible input charge weight" 
              (sub lW (Atomic "min")) kilogram
eqTNTWeight = makeUC "w_TNT" "Explosive Mass in equivalent weight of TNT" 
              (sub lW (Atomic "TNT")) kilogram

----Quantities--
risk_fun    = makeUC "B" "Risk function (short dimension)" cB unitless
glass_type  = makeUC "g" "Glass type, g in {AN, HS, FT}" lG unitless
is_safe1    = makeUC "is_safe1" "True when calculated probability is less than tolerable probability" 
              (Concat [lI,lS,(Atomic "_"),lS,lA,lF,lE,(Atomic "1")]) unitless
is_safe2    = makeUC "is_safe2" "True when load resistance (capacity) is greater than load (demand)" 
              (Concat [lI,lS,(Atomic "_"),lS,lA,lF,lE,(Atomic "2")]) unitless
sdf         = makeUC "J" "Stress distribution factor (Function)" cJ unitless
sdf_tol     = makeUC "J_tol" "Stress distribution factor (Function) based on P_btol"
              (sub cJ (Atomic "tol")) unitless
prob_br     = makeUC "P_b" "Probability of breakage" (sub cP lB) unitless
pb_tol      = makeUC "P_btol" "Tolerable probablity of breakage" 
              (sub cP (Atomic "btol")) unitless
dimlessLoad = makeUC "q_hat" "Dimensionless load" (hat lQ) unitless
tolLoad     = makeUC "q_hat_tol" "Tolerable load" (sub (hat lQ) (Atomic "tol")) unitless

----Acronyms-----
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,instanceMod,likelyChange,
  physSysDescr,requirement,softwareRS,gLassBR,theoreticMod, annealedGlass,
  aspectR,aspectRMax,fullyTGlass,glassTypeFac,heatSGlass,iGlass,lDistriFac,
  lResistance,lShareFac,notApp,nonFactorL,eqTNT]
  
assumption,dataDefn,genDefn,goalStmt,instanceMod,likelyChange,
  physSysDescr,requirement,softwareRS,gLassBR,theoreticMod, annealedGlass,
  aspectR,aspectRMax,fullyTGlass,glassTypeFac,heatSGlass,iGlass,lDistriFac,
  lResistance,lShareFac,notApp,nonFactorL,eqTNT :: ConceptChunk
assumption    = makeCC "A" "Assumption"
dataDefn      = makeCC "DD" "Data Definition"
genDefn       = makeCC "GD" "General Definition"
goalStmt      = makeCC "GS"  "Goal Statement"
instanceMod   = makeCC "IM" "Instance Model"
likelyChange  = makeCC "LC" "Likely Change"
physSysDescr  = makeCC "PS" "Physical System Description"
requirement   = makeCC "R" "Requirement"
softwareRS    = makeCC "SRS" "Software Requirements Specification"
gLassBR       = makeCC "GlassBR" "Glass-BR"
theoreticMod  = makeCC "T" "Theoretical Model"
annealedGlass = makeCC "AN" "Annealed Glass"
aspectR       = makeCC "AR" "Aspect Ratio"
aspectRMax    = makeCC "AR_max" "Maximum Aspect Ratio"
fullyTGlass   = makeCC "FT" "Fully Tempered Glass"
glassTypeFac  = makeCC "GTF" "Glass Type Factor"
heatSGlass    = makeCC "HS" "Heat Strengthened Glass"
iGlass        = makeCC "IG" "Insulating Glass"
lDistriFac    = makeCC "LDF" "Load Distribution Factor"
lResistance   = makeCC "LR" "Load Resistance"
lShareFac     = makeCC "LSF" "Load Share Factor"
notApp        = makeCC "N/A" "Not Applicable"
nonFactorL    = makeCC "NFL" "Non-Factored Load"
eqTNT         = makeCC "TNT" "TNT (Trinitrotoluene) Equivalent Factor"


----Terminology---- 
terms :: [ConceptChunk]
terms = [aR, gbr, lite, glassTy, an, ft, hs, gtf, lateral, load, specDeLoad, lr, 
  ldl, nfl, glassWL, sdl, lsf, pb, specA, blaReGLa, eqTNTChar, sD]

aR, gbr, lite, glassTy, an, ft, hs, gtf, lateral, load, specDeLoad, lr, 
  ldl, nfl, glassWL, sdl, lsf, pb, specA, blaReGLa, eqTNTChar, sD :: ConceptChunk
aR            = makeCC "Aspect ratio (AR)" "It is the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5."
gbr           = makeCC "Glass breakage" "The fracture or breakage of any lite or ply in monolithic, laminated, or insulating glass."
lite          = makeCC "Lite" "Pieces of glass that are cut, prepared, and used to create the window or door."
glassTy       = makeCC "Glass Types" ""
an            = makeCC "Annealed (AN) glass" "A at, monolithic, glass lite which has uniform thickness where the residual surface stresses are almost zero."
ft            = makeCC "Fully tempered (FT) glass" "A at and monolithic, glass lite of uniform thickness that has been subjected to a special heat treatment process where the residual surface compression is not less than 69 MPa (10 000 psi) or the edge compression not less than 67 MPa (9700 psi)."
hs            = makeCC "Heat strengthened (HS) glass" "A at, monolithic, glass lite of uniform thickness that has been subjected to a special heat treatment process where the residual surface compression is not less than 24 MPa (3500psi) or greater than 52 MPa (7500 psi)."
gtf           = makeCC "Glass type factor (GTF)" "A multiplying factor for adjusting the LR of different glass type, that is, AN, HS, or FT in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions."
lateral       = makeCC "Lateral" "Perpendicular to the glass surface"
load          = makeCC "Load" "A uniformly distributed lateral pressure."
specDeLoad    = makeCC "Specified design load" "The magnitude in kPa (psf), type (for example, wind or snow) and duration of the load given by the specifying authority."
lr            = makeCC "Load resistance" "The uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration."
ldl           = makeCC "Load duration load" "Any load lasting approximately 30 days."
nfl           = makeCC "Non-factored load (NFL)" "Three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass."
glassWL       = makeCC "Glass weight load" "The dead load component of the glass weight."
sdl           = makeCC "Short duration load" "Any load lasting 3s or less."
lsf           = makeCC "Load share (LSF) factor" "A multiplying factor derived from the load sharing between the double glazing, of equal or different thickness's and types (including the layered behaviour of LG under long duration loads), in a sealed IG unit."
pb            = makeCC "Probability of breakage Pb" "The fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000."
specA         = makeCC "Specifying authority" "The design professional responsible for interpreting applicable regulations of authorities having jurisdiction and considering appropriate site specific factors to determine the apprpriate values used to calculate the specified design load, and furnishing other information required to perform this practice."
blaReGLa      = makeCC "Blast resistant glazing" "Glazing that provides protection against air blast pressure generated by explosions."
eqTNTChar     = makeCC "Equivalent TNT charge mass" "Mass of TNT placed on the ground in a hemisphere that represents the design explosive threat."
sD            = makeCC "Stand off distance (SD)" "The distance from the glazing surface to the centroid of a hemispherical high explosive charge."


----Variables----
variables :: [VarChunk]
variables = [lRe, nonFL, glaTyFac, loadSF, loadDF]

lRe, nonFL, glaTyFac, loadSF, loadDF :: VarChunk
lRe       = makeVC "LR" "Load Resistance" (Concat [cL,cR])
nonFL     = makeVC "NFL" "Non-Factored Load" (Concat [cN,cF,cL])
glaTyFac  = makeVC "GTF" "Glass Type Factor" (Concat [cG,cT,cF])
loadSF    = makeVC "LSF" "Load Share Factor" (Concat [cL,cS,cF])
loadDF    = makeVC "LDF" "Load Duration Factor" (Concat [cL,cD,cF])

----EqChunks----
--Theoretical models--
t1SafetyReq :: RelationChunk
t1SafetyReq = makeRC "Safety Requirement-1" t1descr safety_require1_rel

safety_require1_rel :: Relation
safety_require1_rel = (C is_safe1) := (C prob_br)

--relation within relation

t1descr :: Sentence
t1descr = 
  (S "If is_safe1 = True, the glass is considered safe. is_safe1 and is_safe2 (from T2) " :+:
     S "are either both True or both False. ") :+: (U $ prob_br ^. symbol) :+: 
    (S " is the probability of breakage, as calculated in IM1. ") :+: (U $ pb_tol ^. symbol) :+:
    (S " is the tolerable probability entered by the user.")

t2SafetyReq :: RelationChunk
t2SafetyReq = makeRC "Safety Requirement-2" t2descr safety_require2_rel

safety_require2_rel :: Relation
safety_require2_rel = (C is_safe2) := (C lRe)

--relation within relation

t2descr :: Sentence
t2descr = 
  (S "If is_safe2 = True, the glass is considered safe. is_safe1 (from T1) and is_safe2 " :+:
     S "are either both True or both False. ") :+: (U $ lRe ^. symbol) :+:
  (S " is the Load Resistance (also called capacity, as defined in IM2. ") :+: 
    (U $ demand ^. symbol) :+: (S " (also referred as the demand) is " :+:
     S "the 3 second equivalent pressure, as defined in IM3.")

--Instance Models--
probOfBr :: RelationChunk
probOfBr = makeRC "Probability of Glass Breakage" pbdescr pb_rel 

pb_rel :: Relation
pb_rel = (C prob_br) := 1 - (V "e") :^ (Neg (V "B"))  -- equation for B?

pbdescr :: Sentence
pbdescr =
  ((U $ prob_br ^. symbol) :+: S " is the calculated probability of breakage. " :+:
    S "B is the risk of failure. " :+: (U $ sflawParamM ^. symbol) :+: S ", " :+:
    (U $ sflawParamK ^.symbol) :+: S " are the surface flaw parameters. " :+: 
    (U $ plate_len ^.symbol) :+: S ", " :+: (U $ plate_width^.symbol) :+: S " are " :+:
    S "dimensions of the plate, where (" :+: (U $ plate_len ^.symbol) :+: S " > " :+:
    (U $ plate_width ^.symbol) :+: S "). " :+: (U $ mod_elas ^.symbol) :+: S " is the " :+:
    S "modulus of elasticity. " :+: (U $ act_thick ^. symbol) :+: S " is the true " :+:
    S "thickness, which is based on the nominal thickness. " :+: (U $ loadDF ^. symbol) :+:
    S " is the Load Duration Factor. " :+: (U $ sdf ^. symbol) :+: S " is the stress " :+:
    S "distribution factor.")

calOfCap :: RelationChunk
calOfCap = makeRC "Calculation of Capacity(LR)" capdescr cap_rel

cap_rel :: Relation
cap_rel = (C lRe) := ((C nonFL):*(C glaTyFac):*(C loadSF)) 

capdescr :: Sentence
capdescr =
  (S "LR is the Load Resistance, which is also called capacity. " :+:
   S "NFL is the Non-Factored Load. " :+: S "GTF is the Glass Type Factor. " :+: 
   S "LSF is the Load Share Factor.")

calOfDe :: RelationChunk
calOfDe = makeRC "Calculation of Demand(q)" dedescr de_rel

de_rel :: Relation
de_rel = (C demand) := FCall (C demand) [C eqTNTWeight, C sd] 

dedescr :: Sentence
dedescr = 
  ((U $ demand ^. symbol) :+: S " or demand, is the 3 second equivalent pressure ":+:
    S "obtained from the Figure 2 by interpolation using stand off distance (" :+:
    (U $ sd ^. symbol) :+: S ") and " :+: (U $ eqTNTWeight ^. symbol) :+: S " as " :+:
    S "parameters. " :+: (U $ eqTNTWeight ^. symbol) :+: S " is defined as " :+:
    (U $ eqTNTWeight ^. symbol) :+: S " = " :+: (U $ char_weight ^. symbol) :+:
    S " * TNT. " :+: (U $ char_weight ^. symbol) :+: S " is the charge weight. " :+: 
    S "TNT is the TNT equivalent factor. " :+: (U $ sd ^.symbol) :+: S " is the ":+:
    S " stand off distance where " :+: (U $ sd ^. symbol) :+: S " = .") --equation in sentence

--Data Definitions--
hFromt :: RelationChunk
hFromt = makeRC "Minimum Thickness(h) from Nominal Thickness(t)" hFromtdescr hFromt_rel

hFromt_rel :: Relation
hFromt_rel = (C act_thick) := FCall (C act_thick) [C nom_thick]

hFromtdescr :: Sentence
hFromtdescr =
 ((U $ act_thick ^. symbol) :+: S " is a function that maps from the nominal " :+:
  S "thickness (" :+: (U $ nom_thick ^. symbol) :+: S ") to the minimum thickness, " :+:
  S "as follows: h(t) = (t = 2.5 => 2.16 | t = 2.7 => 2.59 | t = 3.0 => 2.92 | t = " :+: 
  S "4.0 => 3.78 | t = 5.0 => 4.57 | t = 6.0 => 5.56 | t = 8.0 => 7.42 | t = 10.0 " :+:
  S "=> 9.02 | t = 12.0 => 11.91 | t = 16.0 => 15.09 | t = 19.0 => 18.26 | t = 22.0 " :+:
  S " => 21.44)")

loadDurFac :: RelationChunk
loadDurFac = makeRC "Load Duration Factor(LDF)" loadDurFacdescr loadDurFac_rel

loadDurFac_rel :: Relation
loadDurFac_rel = (C loadDF) := ((C load_dur):/(Int 60)):^((C sflawParamM):/(Int 16))

loadDurFacdescr :: Sentence
loadDurFacdescr =
  ((U $ load_dur ^.symbol) :+: S " is the duration of the load. " :+: (U $ sflawParamM ^. symbol) :+:
    S " is a surface flaw parameter")

strDisFac :: RelationChunk
strDisFac = makeRC "Stress Distribution Factor(J)" strDisFacdescr strDisFac_rel

strDisFac_rel :: Relation
strDisFac_rel = (C sdf) := FCall (C sdf) [C dimlessLoad, (C plate_len):/(C plate_width)]

strDisFacdescr :: Sentence
strDisFacdescr =
  ((U $ sdf ^. symbol) :+: S " is the stress distribution factor, which is obtained by " :+:
    S "interpolating from the data shown in Figure 3. " :+: (U $ dimlessLoad ^. symbol) :+:
    S " is the dimensionless load. " :+: (U $ plate_len ^.symbol) :+: S ", " :+: 
    (U $ plate_width ^.symbol) :+: S " are dimensions of the plate, where (" :+: 
    (U $ plate_len ^.symbol) :+: S " > " :+: (U $ plate_width ^.symbol) :+: S ").")

nonFacLoad :: RelationChunk
nonFacLoad = makeRC "Non-Factored Load" nonFLdescr nonFL_rel

nonFL_rel :: Relation
nonFL_rel = (C nonFL) := ((C tolLoad):*(C mod_elas):*(C act_thick):^(Int 4)):/(((C plate_len):^2):*((C plate_width):^2))

nonFLdescr :: Sentence
nonFLdescr =
  ((U $ mod_elas ^. symbol) :+: S " is the modulus of elasticity. " :+: (U $ plate_len ^. symbol) :+:
    S ", " :+: (U $ plate_width ^.symbol) :+: S " are the dimensions of the plate where (" :+:
    (U $ plate_len ^. symbol) :+: S " > " :+: (U $ plate_width ^. symbol) :+: S "). " :+:
    (U $ act_thick ^. symbol) :+: S " is the true thickness, which is based on the nominal thickness. " :+:
    (U $ tolLoad ^. symbol) :+: S " is the tolerable pressure.")

gTF :: RelationChunk
gTF = makeRC "Glass Type Factor(GTF)" gTFdescr gTF_rel

gTF_rel :: Relation
gTF_rel = (C glaTyFac) := FCall (C glaTyFac) [C glass_type]

gTFdescr :: Sentence
gTFdescr = 
  ((U $ glaTyFac ^. symbol) :+: S " is a function that maps from the glass type (" :+:
    (U $ glass_type ^. symbol) :+: S ") to a real number, as follows: " :+: (U $ glaTyFac ^.symbol) :+:
    S "(" :+: (U $ glass_type ^. symbol) :+: S ") = (" :+: (U $ glass_type ^. symbol) :+:
    S " = AN => 1.0 |" :+: (U $ glass_type ^. symbol) :+: S " = FT => 4.0|" :+:  
    (U $ glass_type ^. symbol) :+: S " = HS => 2.0). " :+: S "AN is annealed glass. "  :+:
    S "FT is fully tempered glass. " :+: S "HS is heat strengthened glass.")

dL :: RelationChunk
dL = makeRC "Dimensionless Load(q_hat)" dLdescr dL_rel

dL_rel :: Relation
dL_rel = (C tolLoad) := ((C demand):*((C plate_len):^(Int 2)):*((C plate_width):^(Int 2))):/((C mod_elas):*((C act_thick):^(Int 4)):*(C glaTyFac))

dLdescr :: Sentence
dLdescr =
  ((U $ demand ^. symbol) :+: S " is the 3 second equivalent pressure. " :+: 
    (U $ plate_len ^. symbol) :+: S ", " :+: (U $ plate_width ^. symbol) :+: 
    S " are dimensions of the plate, where (" :+: (U $ plate_len ^. symbol) :+:
    S " > " :+: (U $ plate_width ^. symbol) :+: S "). " :+: (U $ mod_elas ^. symbol) :+:
    S " is the modulus of elasticity. " :+: (U $ act_thick ^. symbol) :+: 
    S " is the true thickness, which is based on the nominal thickness. " :+:
    (U $ glaTyFac ^. symbol) :+: S " is the Glass Type Factor.")

tolPre :: RelationChunk
tolPre = makeRC "Tolerable Pressure(q_hat_tol)" tolPredescr tolPre_rel

tolPre_rel :: Relation
tolPre_rel = (C tolLoad) := FCall (C tolLoad) [C sdf_tol, (C plate_len):/(C plate_width)]

tolPredescr :: Sentence
tolPredescr =
  ((U $ tolLoad ^. symbol) :+: S " is the tolerable pressure which is obtained from " :+:
    S "Figure 3 using " :+: (U $ sdf_tol ^. symbol) :+: S " and aspect ratio (" :+:
    (U $ plate_len ^.symbol) :+: S "/" :+: (U $ plate_width ^. symbol) :+:
    S ") as parameters using interpolation.")

tolStrDisFac :: RelationChunk
tolStrDisFac = makeRC "Tolerable Stress Distribution Factor(J_tol)" tolStrDisFacdescr tolStrDisFac_rel

tolStrDisFac_rel :: Relation
tolStrDisFac_rel = (C sdf_tol) := (Int 1) -- logarithm?

tolStrDisFacdescr :: Sentence
tolStrDisFacdescr =
  ((U $ sdf_tol ^. symbol) :+: S " is the stress distribution factor calculated with reference to ":+:
    (U $ pb_tol ^. symbol) :+: S ". " :+: (U $ plate_len ^. symbol) :+: S ", " :+: 
    (U $ plate_width ^.symbol) :+: S " are the dimensions of the plate where (" :+:
    (U $ plate_len ^. symbol) :+: S " > " :+: (U $ plate_width ^. symbol) :+: S "). " :+:
    (U $ act_thick ^. symbol) :+: S " is the true thickness, which is based on the nominal thickness. " :+:
    (U $ sflawParamM ^. symbol) :+: S ", " :+: (U $ sflawParamK ^. symbol) :+: 
    S " are the surface flaw parameters. " :+: (U $ loadDF ^. symbol) :+: S " is the " :+:
    S "Load Duration Factor. " :+: (U $ mod_elas ^. symbol) :+: S " is the modulus of " :+:
    S "elasticity. " :+: (U $ pb_tol ^. symbol) :+: S " is the tolerable probability entered " :+:
    S " by the user.")
