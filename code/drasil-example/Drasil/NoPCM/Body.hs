module Drasil.NoPCM.Body where

import Language.Drasil
import Language.Drasil.Code (CodeSpec, codeSpec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Language.Drasil.Development (UnitDefn, unitWrapper) -- FIXME?

import Control.Lens ((^.))

import Data.Drasil.People (thulasi)
import Data.Drasil.Utils (enumSimple,
  itemRefToSent, makeTMatrix, itemRefToSent, noRefs)

import Data.Drasil.Concepts.Documentation as Doc (inModel,
  requirement, item, assumption, thModel, traceyMatrix, model, output_, quantity, input_, 
  physicalConstraint, condition, property, variable, description, symbol_,
  information, goalStmt, physSyst, problem, definition, srs, content, reference,
  document, goal, purpose, assumpDom, funcReqDom, likeChgDom, unlikeChgDom, srsDomains)

import qualified Data.Drasil.Concepts.Math as M (ode, de, unit_, equation)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Phrase (for)
import Data.Drasil.Concepts.Thermodynamics (ener_src, thermal_analysis, temp,
  thermal_energy, ht_trans_theo, heat, melt_pt, boil_pt, ht_flux,
  heat_cap_spec, thermal_conduction)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heat_cap_spec, ht_flux)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Software.Products (compPro)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt)

import qualified Drasil.DocLang.SRS as SRS (probDesc, goalStmt, inModelLabel,
  funcReq, likeChg, unlikeChg)
import Drasil.DocLang (DocDesc, Fields, Field(..), Verbosity(Verbose), 
  InclUnits(IncludeUnits), SCSSub(..), DerivationDisplay(..), SSDSub(..),
  SolChSpec(..), SSDSec(..), DocSection(..),
  IntroSec(IntroProg), IntroSub(IOrgSec, IScope, IChar, IPurpose), Literature(Lit, Doc'),
  RefSec(RefProg), RefTab(TAandA, TUnits), TraceabilitySec(TraceabilityProg),
  TSIntro(SymbOrder, SymbConvention, TSPurpose), dataConstraintUncertainty,
  inDataConstTbl, intro, mkDoc, mkEnumSimpleD, outDataConstTbl, physSystDesc,
  reqF, termDefnF, tsymb, valsOfAuxConstantsF, getDocDesc, egetDocDesc)
import qualified Drasil.DocumentLanguage.Units as U (toSentence) 
import Data.Drasil.SentenceStructures (showingCxnBw, foldlSent_, sAnd,
  isThe, sOf, ofThe, foldlSPCol, foldlSent, foldlSP)

-- Since NoPCM is a simplified version of SWHS, the file is to be built off
-- of the SWHS libraries.  If the source for something cannot be found in
-- NoPCM, check SWHS.
import Drasil.SWHS.Assumptions (newA1, newA2, newA3, newA7, newA8, newA9,
  newA11, newA12, newA14, newA15, newA20)
import Drasil.SWHS.Body (charReader1, charReader2, dataContMid, genSystDesc, 
  orgDocIntro, physSyst1, physSyst2, traceFig1, traceFig2, traceIntro2, traceTrailing)
import Drasil.SWHS.Changes (chgsStart, likeChgTCVOD, likeChgTCVOL, likeChgTLH)
import Drasil.SWHS.Concepts (acronyms, coil, progName, sWHT, tank, tank_para, transient, water)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd1HtFluxCQD)
import Drasil.SWHS.IMods (eBalanceOnPCM, heatEInWtr)
import Drasil.SWHS.References (incroperaEtAl2007, koothoor2013, lightstone2012, 
  parnasClements1986, smithLai2005)
import Drasil.SWHS.Requirements (nonFuncReqs)
import Drasil.SWHS.TMods (consThermE)
import Drasil.SWHS.Unitals (coil_HTC, coil_HTC_max, coil_HTC_min, coil_SA, 
  coil_SA_max, deltaT, diam, eta, ht_flux_C, ht_flux_in, ht_flux_out, htCap_L, 
  htCap_W, htCap_W_max, htCap_W_min, htTransCoeff, in_SA, out_SA, sim_time, 
  tank_length, tank_length_max, tank_length_min, tank_vol, tau, tau_W, temp_C, 
  temp_env, temp_W, thFluxVect, time_final, time_final_max, vol_ht_gen, w_density, 
  w_density_max, w_density_min, w_E, w_mass, w_vol)

import Drasil.NoPCM.DataDesc (inputMod)
import Drasil.NoPCM.Definitions (ht_trans, srs_swhs)
import Drasil.NoPCM.GenDefs (rocTempSimp, swhsGDs)
import Drasil.NoPCM.IMods (eBalanceOnWtr)
import Drasil.NoPCM.Unitals (temp_init)

-- This defines the standard units used throughout the document
this_si :: [UnitDefn]
this_si = map unitWrapper [metre, kilogram, second] ++ map unitWrapper [centigrade, joule, watt]

check_si :: [UnitDefn]
check_si = collectUnits nopcm_SymbMap symbTT 

-- This contains the list of symbols used throughout the document
nopcm_Symbols :: [DefinedQuantityDict]
nopcm_Symbols = (map dqdWr nopcm_Units) ++ (map dqdWr nopcm_Constraints)
 ++ map dqdWr [temp_W, w_E]
 ++ [gradient, uNormalVect] ++ map dqdWr [surface]
  
nopcm_SymbolsAll :: [QuantityDict] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcm_HTC and pcm_SA
                               --FOUND LOC OF ERROR: Instance Models
nopcm_SymbolsAll = (map qw nopcm_Units) ++ (map qw nopcm_Constraints) ++
  (map qw [temp_W, w_E]) ++
  (map qw specParamValList) ++ 
  (map qw [coil_SA_max]) ++ (map qw [tau_W]) ++ 
  (map qw [surface]) ++ (map qw [uNormalVect, gradient, eta])

nopcm_Units :: [UnitaryConceptDict]
nopcm_Units = map ucw [density, tau, in_SA, out_SA,
  htCap_L, QT.ht_flux, ht_flux_in, ht_flux_out, vol_ht_gen,
  htTransCoeff, mass, tank_vol, QT.temp, QT.heat_cap_spec,
  deltaT, temp_env, thFluxVect, time, ht_flux_C,
  vol, w_mass, w_vol, tau_W]

nopcm_Constraints :: [UncertQ]
nopcm_Constraints =  [coil_SA, htCap_W, coil_HTC, temp_init,
  time_final, tank_length, temp_C, w_density, diam]
  -- w_E, temp_W

probDescription, termAndDefn, physSystDescription, goalStates,
  reqS, funcReqs, likelyChgsSect, unlikelyChgsSect, specParamVal :: Section


-------------------
--INPUT INFORMATION
-------------------

--------------------------------
--Section 1 : REFERENCE MATERIAL
--------------------------------
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, 
  tsymb [TSPurpose, SymbConvention [Lit (nw ht_trans), Doc' (nw progName)], SymbOrder],
  TAandA]) :
  IntroSec (IntroProg (introStart ener_src energy progName)
    (introEnd progName program)
  [IPurpose (purpDoc progName),
  IScope (scopeReqStart thermal_analysis sWHT) (scopeReqEnd temp thermal_energy
    water),
  IChar (charReader1 ht_trans_theo) (charReader2 M.de) EmptyS,
  IOrgSec orgDocIntro inModel SRS.inModelLabel (orgDocEnd inModel M.ode progName)]) :
  Verbatim genSystDesc:
  SSDSec 
    (SSDProg [SSDSubVerb probDescription
      , SSDSolChSpec 
        (SCSProg 
          [ Assumptions 
          , TMs ([Label] ++ stdFields) [consThermE] -- only have the same T1 with SWHS
          , GDs ([Label, Units] ++ stdFields) swhsGDs ShowDerivation
          , DDs ([Label, Symbol, Units] ++ stdFields) [dd1HtFluxC] ShowDerivation
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
            [eBalanceOnWtr, heatEInWtr] ShowDerivation
          , Constraints  EmptyS dataConstraintUncertainty dataContMid
            [dataConstTable1, dataConstTable2]
          ]
        )
      ]
    ):
  map Verbatim [reqS, likelyChgsSect, unlikelyChgsSect] ++
  TraceabilitySec
    (TraceabilityProg traceRefList traceTrailing (map LlC traceRefList ++
  (map UlC traceIntro2) ++ [LlC traceFig1, LlC traceFig2]) []) :
  map Verbatim [specParamVal] ++ (Bibliography : [])

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

nopcm_si :: SystemInformation
nopcm_si = SI {
  _sys = srs_swhs,
  _kind = srs,
  _authors = [thulasi],
  _units = check_si,
  _quants = symbTT,
  _concepts = nopcm_Symbols,
  _definitions = [dd1HtFluxCQD],          --dataDefs
  _datadefs = [dd1HtFluxC],
  _inputs = (map qw nopcm_Constraints ++ map qw [temp_W, w_E]), --inputs ++ outputs?
  _outputs = (map qw [temp_W, w_E]),     --outputs
  _defSequence = [Parallel dd1HtFluxCQD []],
  _constraints = (map cnstrw nopcm_Constraints ++ map cnstrw [temp_W, w_E]),        --constrained
  _constants = [],
  _sysinfodb = nopcm_SymbMap,
  _refdb = nopcmRefDB
}

nopcmRefDB :: ReferenceDB
nopcmRefDB = rdb assumps_Nopcm_list_new referencesRefList (reqs ++
  likelyChgs ++ unlikelyChgs) -- FIXME: Convert the rest to new chunk types

nopcm_code :: CodeSpec
nopcm_code = codeSpec nopcm_si [inputMod]
-- Sub interpolation mod into list when possible              ^

nopcm_srs :: Document
nopcm_srs = mkDoc mkSRS (for) nopcm_si

nopcm_SymbMap :: ChunkDB
nopcm_SymbMap = cdb nopcm_SymbolsAll (map nw nopcm_Symbols ++ map nw acronyms) (map cw nopcm_Symbols ++ srsDomains)
  this_si

printSetting :: PrintingInformation
printSetting = PI nopcm_SymbMap defaultConfiguration

assumps_Nopcm_list_new :: [AssumpChunk]
assumps_Nopcm_list_new = [newA1, newA2, newA3, newA5NoPCM, newA6NoPCM,
  newA7, newA8, newA9, newA9NoPCM, newA14, newA15, newA16, newA19, newA20]

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) nopcm_SymbMap

--------------------------
--Section 2 : INTRODUCTION
--------------------------

introStart :: ConceptChunk -> UnitalChunk -> CI-> Sentence
introStart es en pro = foldlSent [S "Due to increasing cost, diminishing",
  S "availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  plural es `sAnd` phrase en +:+. S "storage technology", 
  at_start' pro, S "provide a novel way of storing", phrase en]

introEnd :: CI -> ConceptChunk -> Sentence
introEnd pro pr = foldlSent_ [EmptyS +:+. plural pro, S "The developed",
  phrase pr, S "will be referred to as", titleize pro,
  sParen (short pro)]

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------

purpDoc :: CI -> Sentence
purpDoc pro = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase pro, S "The", plural Doc.goal `sAnd` plural thModel,
  S "used in the", short pro, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption, S "and unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------

scopeReqStart :: ConceptChunk -> ConceptChunk -> Sentence
scopeReqStart ta sw = foldlSent_ [phrase ta, S "of a single", phrase sw]

scopeReqEnd :: ConceptChunk -> ConceptChunk -> ConceptChunk -> Sentence
scopeReqEnd tem te wa = foldlSent_ [S "predicts the",
  phrase tem `sAnd` phrase te,
  S "histories for the", phrase wa]

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------
          
---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------

orgDocEnd :: CI -> CI -> CI -> Sentence
orgDocEnd im_ od pro = foldlSent_ [S "The", phrase im_,
  sParen (makeRefS SRS.inModelLabel),
  S "to be solved is referred to as" +:+. makeRefS eBalanceOnWtr,
  S "The", phrase im_, S "provides the",
  titleize od, sParen (short od), S "that model the"
  +:+. phrase pro, short pro, S "solves this", short od]

----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

--ALL OF THIS SECTION IS NOW PULLED FROM SWHS

--TODO: If/when system constraints recieves any content, add s3_3_intro

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------
  
------------------------------------
--Section 3.2 : USER CHARACTERISTICS
------------------------------------

----------------------------------
--Section 3.3 : SYSTEM CONSTRAINTS
----------------------------------

--s3_3_intro = Paragraph $ EmptyS

--TODO: Placeholder value until content can be added

-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections

-----------------------------------
--Section 4.1 : PROBLEM DESCRIPTION
-----------------------------------

probDescription = SRS.probDesc [probDescIntro progName compPro water sWHT]
  [termAndDefn, physSystDescription, goalStates]

probDescIntro :: CI -> NamedChunk -> ConceptChunk -> ConceptChunk -> Contents
probDescIntro pro cp wa sw = foldlSP [getAcc pro, S "is a",
  phrase cp, S "developed to investigate",
  S "the heating of", phrase wa, S "in a", phrase sw]

termAndDefn = termDefnF Nothing [termAndDefnBullets]

termAndDefnBullets :: Contents
termAndDefnBullets = UlC $ ulcc $ Enumeration $ Bullet $ noRefs $ 
  map (\x -> Flat $
  at_start x :+: S ":" +:+ (x ^. defn))
  [ht_flux, heat_cap_spec, thermal_conduction, transient]
  
physSystDescription = physSystDesc (getAcc progName) fig_tank
  [physSystDescList, LlC fig_tank]

fig_tank :: LabelledContent
fig_tank = llcc (mkLabelRAFig "Tank") $ fig (at_start sWHT `sC` S "with" +:+ phrase ht_flux +:+
  S "from" +:+ phrase coil `sOf` ch ht_flux_C)
  "TankWaterOnly.png"

physSystDescList :: Contents
physSystDescList = enumSimple 1 (short physSyst) $ map foldlSent_
  [physSyst1 tank water, physSyst2 coil tank ht_flux_C]

goalStates = SRS.goalStmt [goalStatesIntro temp coil temp_W, goalStatesList temp_W w_E]
  []

goalStatesIntro :: NamedIdea c => ConceptChunk -> ConceptChunk -> c -> Contents
goalStatesIntro te co temw = foldlSPCol [S "Given", phrase te `ofThe`
  phrase co `sC` S "initial", phrase temw  `sC` S "and material",
  plural property `sC` S "the", phrase goalStmt, S "are"]

goalStatesList :: (NamedIdea a, NamedIdea b) => a -> b -> Contents
goalStatesList temw we = enumSimple 1 (short goalStmt) [
  (S "predict the" +:+ phrase temw +:+ S "over time"),
  (S "predict the" +:+ phrase we +:+ S "over time")]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------
  
  {--end = foldlSent [S "The", phrase uncertCol,
    S "provides an estimate of the confidence with which the physical",
    plural quantity, S "can be measured. This", phrase information,
    S "would be part of the input if one were performing an",
    phrase uncertainty, S "quantification exercise"]-}

-- FIXME: Remove the newA*NoPCM AssumpChunk's once SWHS has migrated to
-- ConceptInstance and SCSProg's Assumptions has been migrated to using
-- assumpDom
  
assumpS3, assumpS4, assumpS5, assumpS9_npcm, assumpS12, assumpS13 :: Sentence
assumpDWCoW, assumpSHECoW, assumpCTNTD, assumpNIHGBW, assumpAPT :: ConceptInstance

assumpS3 = 
  (foldlSent [S "The", phrase water, S "in the", phrase tank,
  S "is fully mixed, so the", phrase temp_W `isThe`
  S "same throughout the entire", phrase tank, sSqBr (makeRefS rocTempSimp)])

assumpS4 = 
  (foldlSent [S "The", phrase w_density, S "has no spatial variation; that is"
  `sC` S "it is constant over their entire", phrase vol, sSqBr ((makeRefS rocTempSimp)`sC`
  (makeRefS likeChgTCVOD))])

newA5NoPCM :: AssumpChunk
newA5NoPCM = assump "Density-Water-Constant-over-Volume" assumpS4 
  (mkLabelRAAssump' "Density-Water-Constant-over-Volume")

assumpDWCoW = cic "assumpDWCoW" assumpS4
  "Density-Water-Constant-over-Volume" assumpDom

assumpS5 = 
  (foldlSent [S "The", phrase htCap_W, S "has no spatial variation; that", 
  S "is, it is constant over its entire", phrase vol, sSqBr (makeRefS rocTempSimp)])

newA6NoPCM :: AssumpChunk
newA6NoPCM = assump "Specific-Heat-Energy-Constant-over-Volume" assumpS5 
  (mkLabelRAAssump' "Specific-Heat-Energy-Constant-over-Volume") 

assumpSHECoW = cic "assumpSHECoW" assumpS5
  "Specific-Heat-Energy-Constant-over-Volume" assumpDom

assumpS9_npcm = 
  (foldlSent [S "The", phrase model, S "only accounts for charging",
  S "of the tank" `sC` S "not discharging. The", phrase temp_W, S "can only",
  S "increase, or remain constant; it cannot decrease. This implies that the",
  phrase temp_init, S "is less than (or equal to) the", phrase temp_C,
  sSqBr ((makeRefS eBalanceOnWtr) `sC` (makeRefS likeChgDT))])

newA9NoPCM :: AssumpChunk
newA9NoPCM = assump "Charging-Tank-No-Temp-Discharge" assumpS9_npcm 
  (mkLabelRAAssump' "Charging-Tank-No-Temp-Discharge")

assumpCTNTD = cic "assumpCTNTD" assumpS9_npcm
  "Charging-Tank-No-Temp-Discharge" assumpDom

assumpS12 = 
  (S "No internal" +:+ phrase heat +:+ S "is generated by the water; therefore, the"
  +:+ phrase vol_ht_gen +:+ S "is zero" +:+.
  sSqBr (makeRefS eBalanceOnWtr))

newA16 :: AssumpChunk
newA16 = assump "No-Internal-Heat-Generation-By-Water" assumpS12 
  (mkLabelRAAssump' "No-Internal-Heat-Generation-By-Water")

assumpNIHGBW = cic "assumpNIHGBW" assumpS12
  "No-Internal-Heat-Generation-By-Water" assumpDom

assumpS13 = 
  (S "The pressure in the" +:+ phrase tank +:+ S "is atmospheric, so the" +:+
  phrase melt_pt `sAnd` phrase boil_pt +:+ S "of water are" +:+ S (show (0 :: Integer))
  :+: Sy (unit_symb QT.temp) `sAnd` S (show (100 :: Integer)) :+:
  Sy (unit_symb QT.temp) `sC` S "respectively" +:+.
  sSqBr ((makeRefS eBalanceOnWtr) `sC` (makeRefS eBalanceOnPCM)))

newA19 :: AssumpChunk
newA19 = assump "Atmospheric-Pressure-Tank" assumpS13 
  (mkLabelRAAssump' "Atmospheric-Pressure-Tank")

assumpAPT = cic "assumpAPT" assumpS13
  "Atmospheric-Pressure-Tank" assumpDom

genDefnDesc2 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> [Sentence]
genDefnDesc2 g_d su vo tfv unv un =
  [S "Applying", titleize g_d, S "to the first term over",
  (phrase su +:+ ch su `ofThe` phrase vo) `sC` S "with",
  ch tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

genDefnDesc3 :: UnitalChunk -> UnitalChunk -> [Sentence]
genDefnDesc3 vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

genDefnDesc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
genDefnDesc5 den ma vo = [S "Using the fact that", ch den :+: S "=" :+:
  ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

genDefnEq1, genDefnEq2, genDefnEq3, genDefnEq4, genDefnEq5 :: Expr

genDefnEq1 = (negate (int_all (eqSymb vol) ((sy gradient) $. (sy thFluxVect)))) + 
  (int_all (eqSymb vol) (sy vol_ht_gen)) $=
  (int_all (eqSymb vol) ((sy density)
  * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

genDefnEq2 = (negate (int_all (eqSymb surface) ((sy thFluxVect) $. (sy uNormalVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $= 
  (int_all (eqSymb vol)
  ((sy density) * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

genDefnEq3 = (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol) $= 
  (int_all (eqSymb vol) ((sy density) * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

genDefnEq4 = (sy density) * (sy QT.heat_cap_spec) * (sy vol) * deriv
  (sy QT.temp) time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol)

genDefnEq5 = (sy mass) * (sy QT.heat_cap_spec) * deriv (sy QT.temp)
  time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out)
  * (sy out_SA) + (sy vol_ht_gen) * (sy vol)

--TODO: Implement physical properties of a substance

iModDesc1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UncertQ -> ConceptChunk -> UnitalChunk -> UncertQ -> ConceptChunk ->
  ConceptChunk -> Contents -> UnitalChunk -> Contents -> [Sentence]
iModDesc1 roc temw en wa vo wv ma wm hcw ht hfc csa ta purin _ vhg _ =
  [S "To find the", phrase roc `sOf` ch temw `sC`
  S "we look at the", phrase en, S "balance on" +:+.
  phrase wa, S "The", phrase vo, S "being considered" `isThe`
  phrase wv, ch wv `sC` S "which has", phrase ma +:+.
  (ch wm `sAnd` (phrase hcw `sC` ch hcw)),
  at_start ht, S "occurs in the water from the coil as", (ch hfc
  `sC` S "over area") +:+. ch csa, S "No",
  phrase ht, S "occurs to", (S "outside" `ofThe`
  phrase ta) `sC` S "since it has been assumed to be",
  phrase purin +:+. sParen (makeRefS newA11), S "Assuming no",
  phrase vhg +:+. (sParen (makeRefS newA12) `sC`
  E (sy vhg $= 0)), S "Therefore, the", phrase M.equation, S "for",
  makeRefS rocTempSimp, S "can be written as"]

iModDesc2 :: DataDefinition -> [Sentence]
iModDesc2 d1hf = [S "Using", (makeRefS d1hf) `sC` S "this can be written as"]

iModDesc3 :: UnitalChunk -> UncertQ -> [Sentence]
iModDesc3 wm hcw = [S "Dividing (3) by", ch wm :+: ch hcw `sC`
  S "we obtain"]

iModDesc4 :: UnitalChunk -> UnitalChunk -> UncertQ -> UncertQ ->
  UncertQ -> [Sentence]
iModDesc4 temw wm hcw chtc csa = [S "Setting", (ch temw :+: S "=" :+:
  ch wm :+: ch hcw :+: S "/" :+: ch chtc :+: ch csa)
  `sC` titleize M.equation, S "(4) can be written in its final form as"]

iModEq1, iModEq2, iModEq3, iModEq4 ::Expr

iModEq1 = (sy w_mass) * (sy htCap_W) * deriv (sy temp_W) time $=
  (sy ht_flux_C) * (sy coil_SA)
 
iModEq2 = (sy w_mass) * (sy htCap_W) * deriv (sy temp_W) time $=
  (sy coil_HTC) * (sy coil_SA) * ((sy temp_C) - (sy temp_W))

iModEq3 = deriv (sy temp_W) time $= ((sy coil_HTC) *
  (sy coil_SA)) / ((sy w_mass) * (sy htCap_W)) * ((sy temp_C) -
  (sy temp_W))

iModEq4 = deriv (sy temp_W) time $= (1 / (sy tau_W)) *
  ((sy temp_C) - (sy temp_W))

dataConstTable1 :: LabelledContent
dataConstTable1 = inDataConstTbl dataConstListIn
-- s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, titleize software +:+
  -- titleize' constraint, S "Typical" +:+ titleize value, titleize uncertainty]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)]
  -- data_constraint_conListIn) (titleize input_ +:+ titleize' variable) True

dataConstListIn :: [UncertQ]
dataConstListIn = [tank_length, diam, coil_SA, temp_C, w_density, htCap_W,
  coil_HTC, temp_init, time_final]

dataConstTable2 :: LabelledContent
dataConstTable2 = outDataConstTbl dataConstListOut
-- s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1)] s4_2_6_conListOut)
  -- (titleize output_ +:+ titleize' variable) True

dataConstListOut :: [ConstrConcept]
dataConstListOut = [temp_W, w_E]

inputVar :: [QuantityDict]
inputVar = map qw dataConstListIn 


--------------------------
--Section 5 : REQUIREMENTS
--------------------------

reqS = reqF [funcReqs, nonFuncReqs]

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

funcReqs = SRS.funcReq funcReqsList [] --TODO: Placeholder values until content can be added

funcReqsList :: [Contents]
funcReqsList = funcReqsListWordsNum

reqFMExpr :: Expr
reqFMExpr = ((sy w_mass) $= (sy w_vol) * (sy w_density) $= (((sy diam) / 2) *
  (sy tank_length) * (sy w_density)))

reqIIV, reqFM, reqCISPC, reqOIDQ, reqCTWOT, reqCCHEWT :: ConceptInstance
reqIIV = cic "reqIIV" (titleize input_ +:+ S "the" +:+ plural quantity +:+
    S "described in" +:+ makeRefS reqIVRTable `sC` S "which define the" +:+
    plural tank_para `sC` S "material" +:+ plural property +:+
    S "and initial" +:+. plural condition) "Input-Inital-Values" funcReqDom
reqFM = cic "reqFM" (S "Use the" +:+ plural input_ +:+ S "in" +:+ makeRefS reqIIV +:+
    S "to find the" +:+ phrase mass +:+ S "needed for" +:+ makeRefS eBalanceOnWtr +:+
    S "to" +:+ makeRefS eBalanceOnPCM `sC` S "as follows, where" +:+ ch w_vol `isThe`
    phrase w_vol +:+ S "and" +:+ (ch tank_vol `isThe` phrase tank_vol) :+:
    S ":" +:+ E reqFMExpr) "Find-Mass" funcReqDom  -- FIXME: Equation shouldn't be inline.
reqCISPC = cic "reqCISPC" (S "Verify that the" +:+ plural input_ +:+
    S "satisfy the required" +:+ phrase physicalConstraint +:+
    S "shown in" +:+. makeRefS dataConstTable1)
    "Check-Inputs-Satisfy-Physical-Constraints" funcReqDom
reqOIDQ = cic "reqOIDQ" (titleize' output_ `sAnd` plural input_ 
    +:+ plural quantity +:+
    S "and derived" +:+ plural quantity +:+ S "in the following list: the" +:+
    plural quantity +:+ S "from" +:+ (makeRefS reqIIV) `sC` S "the" +:+
    phrase mass +:+ S "from" +:+ makeRefS reqFM `sAnd` ch tau_W +:+.
    sParen (S "from" +:+ makeRefS eBalanceOnWtr)) "Output-Input-Derivied-Quantities" funcReqDom
reqCTWOT = cic "reqCTWOT" (S "Calculate and output the" +:+ phrase temp_W +:+
    sParen (ch temp_W :+: sParen (ch time)) +:+ S "over the" +:+
    phrase sim_time) "Calculate-Temperature-Water-Over-Time" funcReqDom
reqCCHEWT = cic "reqCCHEWT" 
    (S "Calculate and" +:+ phrase output_ +:+ S "the" +:+
    phrase w_E +:+ sParen (ch w_E :+: sParen (ch time)) +:+ S "over the" +:+
    phrase sim_time +:+. sParen (S "from" +:+ makeRefS heatEInWtr))
    "Calculate-Change-Heat_Energy-Water-Time" funcReqDom

reqIVRTable :: LabelledContent
reqIVRTable = llcc (mkLabelSame "Input-Variable-Requirements" Tab) $ 
  Table [titleize symbol_, titleize M.unit_, titleize description]
  (mkTable [ch, U.toSentence, phrase] inputVar)
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True

reqs :: [ConceptInstance]
reqs = [reqIIV, reqFM, reqCISPC, reqOIDQ, reqCTWOT, reqCCHEWT]

funcReqsListWordsNum :: [Contents]
funcReqsListWordsNum =
  (mkEnumSimpleD reqs) ++ [LlC reqIVRTable]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS

----------------------------
--Section 6 : LIKELY CHANGES
----------------------------

likelyChgsSect = SRS.likeChg likelyChgsList []

likelyChgsList :: [Contents]
likelyChgsList = mkEnumSimpleD likelyChgs

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgTCVOD, likeChgTCVOL, likeChgDT, likeChgTLH]

likeChgDT :: ConceptInstance
likeChgDT = cic "likeChgDT" (
  (makeRefS newA9NoPCM) :+: S "- The" +:+ phrase model +:+
  S "currently only accounts for charging of the tank. That is, increasing the" +:+ phrase temp +:+
  S "of the water to match the" +:+ phrase temp +:+ S "of the coil. A more complete"  
  +:+ phrase model +:+. S "would also account for discharging of the tank") 
  "Discharging-Tank" likeChgDom
-- likeChg4 = LikelyChange (LCChunk (nw $ npnc "likeChg4" $
  -- nounPhraseSent (makeRefS assump11 :+: S "- Any real" +:+ phrase tank +:+
  -- S "cannot be perfectly insulated and will lose" +:+. phrase heat))
  -- []) EmptyS

---- Empty list is supposed to take a ModuleChunk. Not sure what to put there.
-- likeChg1 = LikelyChange (LCChunk (nw $ npnc "likeChg1" $
  -- nounPhraseSent (makeRefS assump7 :+: S "- The" +:+ phrase temp_C +:+
  -- S "will change over" +:+ (S "course" `ofThe` S "day, depending") +:+
  -- S "on the" +:+ phrase energy +:+ S "received from the sun."))
  -- []) EmptyS
-- likeChg2 = LikelyChange (LCChunk (nw $ npnc "likeChg2" $
  -- nounPhraseSent (makeRefS assump8 :+: S "- The" +:+ phrase temp_C +:+
  -- S "will actually change along its length as the" +:+ phrase water +:+
  -- S "within it cools."))
  -- []) EmptyS

-- s6_likeChg_list = [likeChg1, likeChg2, likeChg3, likeChg4]
-- likeChg1, likeChg2, likeChg3, likeChg4 :: Contents

-- likeChg1 = [s6_start 7, S "The", phrase temp_C,
  -- S "will change over", S "course" `ofThe` S "day, depending",
  -- S "on the", phrase energy, S "received from the sun"]

-- likeChg2 = [s6_start 8, S "The", phrase temp_C,
  -- S "will actually change along its length as the",
  -- phrase water, S "within it cools"]

-- likeChg3 = [s6_start 9, S "The", phrase model +:+.
  -- S "currently only accounts for charging of the tank",
  -- S "A more complete", phrase model, S "would also",
  -- S "account for discharging of the tank"]

-- likeChg4 = [s6_start 11, S "Any real", phrase tank,
  -- S "cannot be perfectly insulated and will lose",
  -- phrase heat]

-------------------------------
--Section 6b : UNLIKELY CHANGES
-------------------------------

unlikelyChgsSect = SRS.unlikeChg unlikelyChgsList []

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikeChgWFS, unlikeChgNIHG]

unlikelyChgsList :: [Contents]
unlikelyChgsList = mkEnumSimpleD unlikelyChgs

unlikeChgWFS :: ConceptInstance
unlikeChgWFS = cic "unlikeChgWFS" (
  foldlSent [chgsStart newA14, S "It is unlikely for the change of",
  phrase water, S "from liquid to a solid, or from liquid to gas to be considered"])
  "Water-Fixed-States" unlikeChgDom

unlikeChgNIHG :: ConceptInstance
unlikeChgNIHG = cic "unlikeChgNIHG" (
  foldlSent [chgsStart newA16, S "Is used for the derivations of",
  makeRefS eBalanceOnWtr] ) "No-Internal-Heat-Generation" unlikeChgDom

----------------------------------------------
--Section 7:  TRACEABILITY MATRICES AND GRAPHS
----------------------------------------------

traceRefList :: [LabelledContent]
traceRefList = [traceTable1, traceTable2, traceTable3]

traceInstaModel, traceData, traceFuncReq, traceLikelyChg, traceDataDefs, traceGenDefs,
  traceAssump, traceTheories :: [String]
traceDataRef, traceFuncReqRef, traceInstaModelRef, traceAssumpRef, traceTheoriesRef,
  traceDataDefRef, traceLikelyChgRef, traceGenDefRef :: [Sentence]

traceInstaModel = ["IM1", "IM2"]
traceInstaModelRef = map makeRefS [eBalanceOnWtr, heatEInWtr]

traceFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
traceFuncReqRef = map makeRefS reqs

traceData = ["Data Constraints"]
traceDataRef = [makeRefS dataConstTable1] --FIXME: Reference section?

traceAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14"]
traceAssumpRef = map makeRefS assumps_Nopcm_list_new

traceTheories = ["T1"]
traceTheoriesRef = map makeRefS [consThermE]

traceGenDefs = ["GD1", "GD2"]
traceGenDefRef = map makeRefS swhsGDs

traceDataDefs = ["DD1"]
traceDataDefRef = map makeRefS [dd1HtFluxC]

traceLikelyChg = ["LC1", "LC2", "LC3", "LC4"]
traceLikelyChgRef = map makeRefS likelyChgs

{-Traceability Matrix 1-}

traceRow1 :: [String]
traceRow1 = traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel

traceRowHeader1 :: [Sentence]
traceRowHeader1 = zipWith itemRefToSent traceRow1
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef)

traceColumns1 :: [[String]]
traceColumns1 = [trace1T1, trace1GD1, trace1GD2, trace1DD1, trace1IM1,
  trace1IM2]

trace1T1, trace1GD1, trace1GD2, trace1DD1, trace1IM1, trace1IM2 :: [String]

--list of each item that "X" item requires for traceability matrix
trace1T1 = []
trace1GD1 = []
trace1GD2 = ["T1"]
trace1DD1 = ["GD1"]
trace1IM1 = ["GD2", "DD1"]
trace1IM2 = []

traceTable1 :: LabelledContent
traceTable1 = llcc (mkLabelSame "TraceyRI" Tab) $
  Table (EmptyS : traceRowHeader1)
  (makeTMatrix (traceRowHeader1) (traceColumns1) (traceRow1))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 2-}

traceRow2 :: [String]
traceRow2 = traceInstaModel ++ traceData ++ traceFuncReq

--column header
traceRowHeader2 :: [Sentence]
traceRowHeader2 = zipWith itemRefToSent traceRow2
  (traceInstaModelRef ++ traceDataRef ++ traceFuncReqRef)

--row header
traceColHeader2 :: [Sentence]
traceColHeader2 = zipWith itemRefToSent (traceInstaModel ++ traceFuncReq)
  (traceInstaModelRef ++ traceFuncReqRef)

traceColumns2 :: [[String]]
traceColumns2 = [trace2IM1, trace2IM2, trace2R1,
  trace2R2, trace2R3, trace2R4, trace2R5, trace2R6]

trace2IM1, trace2IM2, trace2R1, trace2R2,
  trace2R3, trace2R4, trace2R5, trace2R6 :: [String]

--list of each item that "X" item requires for traceability matrix
trace2IM1 = []
trace2IM2 = []
trace2R1 = []
trace2R2 = ["R1","IM1"]
trace2R3 = ["Data Constraints"]
trace2R4 = ["R1", "R2", "IM1"]
trace2R5 = ["IM1"]
trace2R6 = ["IM2"]

traceTable2 :: LabelledContent
traceTable2 = llcc (mkLabelSame "TraceyRIs" Tab) $ Table
  (EmptyS : traceRowHeader2)
  (makeTMatrix (traceColHeader2) (traceColumns2) (traceRow2))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 3-}

traceRowHeader3, traceColHeader3 :: [Sentence]
traceRowHeader3 = zipWith itemRefToSent traceAssump traceAssumpRef

traceColHeader3 = zipWith itemRefToSent
  (traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel ++ traceLikelyChg)
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef ++
  traceLikelyChgRef)

traceColumns3 :: [[String]]
traceColumns3 = [trace3T1, trace3GD1, trace3GD2, trace3DD1,
  trace3IM1, trace3IM2, trace3LC1, trace3LC2, trace3LC3, trace3LC4]

trace3T1, trace3GD1, trace3GD2, trace3DD1,
  trace3IM1, trace3IM2, trace3LC1, trace3LC2, trace3LC3, trace3LC4 :: [String]

trace3T1  = ["A1"]
trace3GD1 = ["A2"]
trace3GD2 = ["A3", "A4", "A5"]
trace3DD1 = ["A6", "A7", "A8"]
trace3IM1 = ["A9", "A10"]
trace3IM2 = ["A10"]
trace3LC1 = ["A7"]
trace3LC2 = ["A8"]
trace3LC3 = ["A9"]
trace3LC4 = ["A11"]

traceTable3 :: LabelledContent
traceTable3 = llcc (mkLabelSame "TraceyAI" Tab) $ Table
  (EmptyS : traceRowHeader3)
  (makeTMatrix traceColHeader3 traceColumns3 traceAssump)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other" +:+
  titleize' item)) True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

------------------------
-- Traceabilty Graphs --
------------------------

-- Using the SWHS graphs as place holders until ones can be generated for NoPCM 

------------------------------------------
--Section 8: SPECIFICATION PARAMETER VALUE
------------------------------------------

specParamValList :: [QDefinition]
specParamValList = [tank_length_min, tank_length_max,
  w_density_min, w_density_max, htCap_W_min, htCap_W_max, coil_HTC_min,
  coil_HTC_max, time_final_max]

specParamVal = valsOfAuxConstantsF progName specParamValList

------------
--REFERENCES
------------

referencesRefList :: BibRef
referencesRefList = [incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, smithLai2005]
