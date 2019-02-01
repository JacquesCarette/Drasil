module Drasil.SSP.Body (ssp_srs, ssp_code, sspSymMap, printSetting) where

import Language.Drasil hiding (organization, Verb)
import Language.Drasil.Code (CodeSpec, codeSpec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Language.Drasil.Development (UnitDefn, unitWrapper) -- FIXME

import Control.Lens ((^.))
import Prelude hiding (sin, cos, tan)
import qualified Data.Map as Map

import Drasil.DocLang (DocDesc, DocSection(..), IntroSec(..), IntroSub(..), 
  LCsSec(..), LFunc(..), RefSec(..), RefTab(..), TConvention(..), --TSIntro, 
  TSIntro(..), UCsSec(..), Fields, Field(..), SSDSec(..), SSDSub(..),
  Verbosity(..), InclUnits(..), DerivationDisplay(..), SolChSpec(..),
  SCSSub(..), GSDSec(..), GSDSub(..), 
  ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub),
  dataConstraintUncertainty, goalStmtF, inDataConstTbl, intro, mkDoc,
  mkEnumSimpleD, outDataConstTbl, probDescF, termDefnF,
  tsymb'', valsOfAuxConstantsF,getDocDesc, egetDocDesc, generateTraceMap,
  getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM, getSCSSub,
  goalStmt_label, physSystDescription_label, generateTraceMap')

import qualified Drasil.DocLang.SRS as SRS (inModel, physSyst, assumpt)

import Data.Drasil.Concepts.Documentation as Doc (analysis, assumption,
  design, document, effect, element, endUser, environment, goalStmt, inModel, 
  input_, interest, interest, interface, issue, loss, method_, organization, 
  physics, problem, product_, property, software, softwareSys, srs, srsDomains,
  sysCont, system, table_, template, user, value, variable, physSyst, doccon,
  doccon')
import Data.Drasil.Concepts.Education (solidMechanics, undergraduate, educon)
import Data.Drasil.Concepts.Math (equation, surface, mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (mass, physicalcon)
import Data.Drasil.Concepts.Physics (fbd, force, strain, stress, physicCon)
import Data.Drasil.Concepts.Software (accuracy, correctness, maintainability, 
  program, reusability, understandability, softwarecon, performance)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce, solidcon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Software.Products (sciCompS, prodtcon)
import Data.Drasil.Quantities.Math as QM (pi_)

import Data.Drasil.People (henryFrankis)
import Data.Drasil.Phrase (for)
import Data.Drasil.SentenceStructures (foldlList, SepType(Comma), FoldType(List), 
  foldlSP, foldlSent, foldlSent_, ofThe, sAnd, sOr, foldlSPCol)
import Data.Drasil.SI_Units (degree, metre, newton, pascal, kilogram, second, derived, fundamentals)
import Data.Drasil.Utils (bulletFlat, bulletNested, enumSimple, noRefsLT)

import Drasil.SSP.Assumptions (newAssumptions)
import Drasil.SSP.Changes (likelyChgs, likelyChanges_SRS, unlikelyChgs,
  unlikelyChanges_SRS)
import Drasil.SSP.DataDefs (dataDefns)
import Drasil.SSP.DataDesc (sspInputMod)
import Drasil.SSP.Defs (acronyms, crtSlpSrf, fs_concept, intrslce, itslPrpty, 
  morPrice, mtrlPrpty, plnStrn, slice, slope, slpSrf, soil, soilLyr, ssa, ssp, sspdef,
  sspdef')
import Drasil.SSP.GenDefs (generalDefinitions)
import Drasil.SSP.Goals (sspGoals)
import Drasil.SSP.IMods (sspIMods)
import Drasil.SSP.References (sspCitations)
import Drasil.SSP.Requirements (sspRequirements, sspInputDataTable)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (fs, index, numbSlices, sspConstrained, sspInputs, 
  sspOutputs, sspSymbols)

--type declarations for sections--
aux_cons :: Section

table_of_symbol_intro :: [TSIntro]

problem_desc, termi_defi, phys_sys_desc, goal_stmt :: Section
goals_list, termi_defi_list, phys_sys_desc_p1, phys_sys_desc_bullets,
  phys_sys_desc_p2 :: Contents


--Document Setup--
this_si :: [UnitDefn]
this_si = map unitWrapper [metre, degree, kilogram, second] ++ map unitWrapper [newton, pascal]

check_si :: [UnitDefn]
check_si = collectUnits sspSymMap symbTT

ssp_si :: SystemInformation
ssp_si = SI {
  _sys = ssa, 
  _kind = srs, 
  _authors = [henryFrankis],
  _units = check_si,
  _quants = sspSymbols,
  _concepts = symbTT,
  _definitions = ([] :: [QDefinition]),
  _datadefs = dataDefns,
  _inputs = map qw sspInputs,
  _outputs = map qw sspOutputs,
  _defSequence = [Parallel (qdFromDD (head dataDefns)) (map qdFromDD (tail dataDefns))],
  _constraints = sspConstrained,
  _constants = [],
  _sysinfodb = sspSymMap,
  _usedinfodb = usedDB,
  _refdb = sspRefDB
}

resourcePath :: String
resourcePath = "../../../datafiles/SSP/"

ssp_srs :: Document
ssp_srs = mkDoc mkSRS for ssp_si
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb'' table_of_symbol_intro TAD, TAandA]) :
  IntroSec (IntroProg startIntro kSent
    [IPurpose prpsOfDoc_p1
    , IScope scpIncl scpEnd
    , IChar (phrase solidMechanics)
      (phrase undergraduate +:+ S "level 4" +:+ phrase Doc.physics)
      EmptyS
    , IOrgSec orgSecStart inModel (SRS.inModel [] [])  orgSecEnd]) :
    --FIXME: issue #235
    (GSDSec $ GSDProg2 [SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList], 
      UsrChars [userCharIntro], SystCons [] []]):
    SSDSec 
      (SSDProg [SSDSubVerb problem_desc
        , SSDSolChSpec 
          (SCSProg 
            [Assumptions 
            ,TMs ([Label] ++ stdFields) [factOfSafety, equilibrium, mcShrStrgth,
             effStress]
            , GDs ([Label, Units] ++ stdFields) generalDefinitions ShowDerivation
            , DDs ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
            , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
             sspIMods ShowDerivation
            , Constraints  EmptyS dataConstraintUncertainty EmptyS
              [data_constraint_Table2, data_constraint_Table3]
            ]
          )
        ]
      ):
    ReqrmntSec (ReqsProg [
    FReqsSub funcReqList,
    NonFReqsSub [accuracy,performance] (ssppriorityNFReqs) -- The way to render the NonFReqsSub is right for here, fixme.
    (S "SSA is intended to be an educational tool")
    (S "")]) :
  [LCsSec $ LCsProg likelyChanges_SRS]
  ++ [UCsSec $ UCsProg unlikelyChanges_SRS] ++ [Verbatim aux_cons] ++ (Bibliography : [])

ssp_label :: TraceMap
ssp_label = Map.union (generateTraceMap mkSRS) (generateTraceMap' $ sspRequirements ++ likelyChgs ++ unlikelyChgs)
 
ssp_refby :: RefbyMap
ssp_refby = generateRefbyMap ssp_label

ssp_datadefn :: [DataDefinition]
ssp_datadefn = getTraceMapFromDD $ getSCSSub mkSRS

ssp_insmodel :: [InstanceModel]
ssp_insmodel = getTraceMapFromIM $ getSCSSub mkSRS

ssp_gendef :: [GenDefn]
ssp_gendef = getTraceMapFromGD $ getSCSSub mkSRS

ssp_theory :: [TheoryModel]
ssp_theory = getTraceMapFromTM $ getSCSSub mkSRS

ssp_assump :: [AssumpChunk]
ssp_assump = newAssumptions

ssp_concins :: [ConceptInstance]
ssp_concins = sspRequirements ++ likelyChgs ++ unlikelyChgs

ssp_section :: [Section]
ssp_section = ssp_sec

ssp_sec :: [Section]
ssp_sec = extractSection ssp_srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
  
ssp_code :: CodeSpec
ssp_code = codeSpec ssp_si [sspInputMod]

ssppriorityNFReqs :: [ConceptChunk]
ssppriorityNFReqs = [correctness, understandability, reusability,
  maintainability]


-- SYMBOL MAP HELPERS --
sspSymMap :: ChunkDB
sspSymMap = cdb (map qw sspSymbols ++ [QM.pi_]) (map nw sspSymbols ++ map nw acronyms ++ map nw [QM.pi_] ++
  map nw doccon ++ map nw prodtcon ++ map nw sspdef ++ map nw sspdef'
  ++ map nw softwarecon ++ map nw physicCon ++ map nw mathcon
  ++ map nw mathcon' ++ map nw solidcon ++ map nw physicalcon
  ++ map nw doccon' ++ map nw derived ++ map nw fundamentals
  ++ map nw educon ++ map nw compcon ++ [nw algorithm, nw ssp] ++ map nw this_si)
  (map cw sspSymbols ++ srsDomains) this_si ssp_label ssp_refby
  ssp_datadefn ssp_insmodel ssp_gendef ssp_theory ssp_assump ssp_concins
  ssp_section []

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT ++ [QM.pi_]) (map nw sspSymbols ++ map nw acronyms ++ map nw [QM.pi_] ++ map nw check_si)
 ([] :: [ConceptChunk]) check_si ssp_label ssp_refby
 ssp_datadefn ssp_insmodel ssp_gendef ssp_theory ssp_assump ssp_concins
 ssp_section []

sspRefDB :: ReferenceDB
sspRefDB = rdb newAssumptions sspCitations (sspRequirements ++
  likelyChgs ++ unlikelyChgs)

printSetting :: PrintingInformation
printSetting = PI sspSymMap defaultConfiguration

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) sspSymMap

-- SECTION 1 --
--automatically generated in mkSRS -

-- SECTION 1.1 --
--automatically generated in mkSRS

-- SECTION 1.2 --
--automatically generated in mkSRS using the intro below

table_of_symbol_intro = [TSPurpose, TypogConvention [Verb $ foldlSent_
  [plural value, S "with a subscript", ch index, S "implies that the",
  phrase value, S "will be taken at and analyzed at a", phrase slice
  `sOr` phrase slice, S "interface composing the total slip", phrase mass]]]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [S "A", phrase slope, S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock, is subject", 
  S "to the influence of gravity on the" +:+. phrase mass, S "For an unstable",
  phrase slope, S "this can cause instability in the form" +:+.
  S "of soil/rock movement", S "The", plural effect,
  S "of soil/rock movement can range from inconvenient to",
  S "seriously hazardous, resulting in signifcant life and economic" +:+.
  plural loss, at_start slope, S "stability is of", phrase interest,
  S "both when analyzing natural", plural slope `sC`
  S "and when designing an excavated" +:+.  phrase slope, at_start ssa,
  S "is", (S "assessment" `ofThe` S "safety of a" +:+ phrase slope) `sC`
  S "identifying the", phrase surface,
  S "most likely to experience slip" `sAnd`
  S "an index of its relative stability known as the", phrase fs]

kSent = keySent ssa

keySent :: (Idea a) => a -> Sentence
keySent pname = foldlSent_ [S "a", phrase pname +:+. phrase problem,
  S "The developed", phrase program, S "will be referred to as the",
  introduceAbb pname, phrase program]
  
-- SECTION 2.1 --
-- Purpose of Document automatically generated in IPurpose
prpsOfDoc_p1 :: Sentence
prpsOfDoc_p1 = purposeDoc ssa crtSlpSrf fs how introduces analysizes
  where how = S "assessing the stability of a" +:+ phrase slope +:+
          phrase design
        introduces = phrase slope +:+ S "stability" +:+ plural issue
        analysizes = S "safe" +:+ phrase slope

purposeDoc :: (Idea a, NamedIdea b, NamedIdea c) =>
              a -> b -> c -> Sentence -> Sentence -> Sentence
              -> Sentence
purposeDoc pname what calculates how introduces analysizes =
  foldlSent [S "The", short pname, phrase program,
  S "determines the", phrase what `sC` S "and its respective",
  phrase calculates, S "as a", phrase method_,
  S "of" +:+. how, S "The", phrase program,
  S "is intended to be used as an educational tool for",
  S "introducing", introduces `sC` S "and will facilitate the",
  phrase analysis `sAnd` phrase design, S "of a", analysizes]

-- SECTION 2.2 --
-- Scope of Requirements automatically generated in IScope
scpIncl, scpEnd :: Sentence
scpIncl = S "stability analysis of a 2 dimensional" +:+ phrase slope `sC`
  S "composed of homogeneous" +:+ plural soilLyr
scpEnd  = S "identifies the most likely failure" +:+
  phrase surface +:+ S "within the possible" +:+ phrase input_ +:+ 
  S "range" `sC` S "and finds the" +:+ phrase fs +:+ S "for the" +:+
  phrase slope

-- SECTION 2.3 --
-- Characteristics of the Intended Reader generated in IChar

-- SECTION 2.4 --
-- Organization automatically generated in IOrgSec
orgSecStart, orgSecEnd :: Sentence
orgSecStart = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an",
  short srs, S "for", phrase sciCompS,
  S "proposed by Koothoor as well as Smith and Lai"]
orgSecEnd   = S "The" +:+ plural inModel +:+ S "provide the set of" +:+
  S "algebraic" +:+ plural equation +:+ S "that must be solved iteratively"
  +:+ S "to perform a" +:+ titleize morPrice +:+ titleize analysis

-- SECTION 3 --
-- SECTION 3.1 --
-- System Context automatically generated
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRef2S sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` S "the", phrase user, S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself" +:+. (sParen $ short ssp),
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]
   
sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phrase product_, S "and the", phrase user,
   S "is through a user" +:+. phrase interface,
   S "The responsibilities of the", phrase user, S "and the", phrase system,
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide the input data related to the soil layer(s) and water" +:+
  S "table (if applicable), ensuring no errors in the data entry",
  S "Ensure that consistent units are used for input variables",
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+ sParen ( 
  makeRef2S $ SRS.assumpt ([]::[Contents]) ([]::[Section])) +:+ S "are appropriate for any particular" +:+
  phrase problem +:+ S "input to the" +:+ phrase software]
  
sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters" +:+ 
  S " input instead of a floating point number",
  S "Determine if the inputs satisfy the required physical and software constraints",
  S "Identify the most likely failure surface within the possible input range",
  S "Find the factor of safety for the slope"]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short ssp +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

-- SECTION 3.2 --
-- User Characteristics automatically generated with the
-- userContraints intro below

userCharIntro :: Contents
userCharIntro = userChar ssa [S "Calculus", titleize Doc.physics]
  [phrase soil, plural mtrlPrpty]

userChar :: (Idea a) => a -> [Sentence] -> [Sentence] -> Contents
userChar pname understandings familiarities = foldlSP [
  S "The", phrase endUser, S "of", short pname,
  S "should have an understanding of undergraduate Level 1",
  foldlList Comma List understandings `sC`
  S "and be familiar with", foldlList Comma List familiarities]

-- SECTION 3.2 --
-- System Constraints automatically generated

-- SECTION 4 --

-- SECTION 4.1 --
problem_desc = probDescF EmptyS ssa ending [termi_defi, phys_sys_desc, goal_stmt]
  where ending = foldlSent_ [S "evaluate the", phrase fs, S "of a",
          phrase's slope, phrase slpSrf, S "and identify the",
          phrase crtSlpSrf, S "of the", phrase slope]

-- SECTION 4.1.1 --
termi_defi = termDefnF Nothing [termi_defi_list]

termi_defi_list = UlC $ ulcc $ Enumeration $ Simple $ noRefsLT $
  map (\x -> (titleize $ x, Flat $ x ^. defn))
  [fs_concept, crtSlpSrf, stress, strain, normForce, shearForce, plnStrn]
  -- most of these are in concepts (physics or solidMechanics)
  -- except for crtSlpSrf & plnStrn which is in defs.hs
  -- and fs which is in Unitals.hs

-- SECTION 4.1.2 --
phys_sys_desc = SRS.physSyst
  [phys_sys_desc_p1, phys_sys_desc_bullets, phys_sys_desc_p2,
   LlC fig_indexconv, LlC fig_forceacting] []

phys_sys_desc_p1 = physSystIntro slope how intrslce slice 
  (S "slice base") fig_indexconv
  where how = S "as a series of" +:+ phrase slice +:+. plural element

physSystIntro :: (NamedIdea a, NamedIdea b, NamedIdea c, HasShortName d, Referable d) =>
  a -> Sentence -> b -> c -> Sentence -> d -> Contents
physSystIntro what how p1 p2 p3 indexref = foldlSP [
  at_start analysis, S "of the", phrase what, S "is performed by looking at",
  plural property, S "of the", phrase what, how, S "Some", plural property,
  S "are", phrase p1, plural property `sC` S "and some are", phrase p2 `sOr`
  p3 +:+. plural property, S "The index convention for referencing which",
  phrase p1 `sOr` phrase p2, S "is being used is shown in", makeRef2S indexref]

phys_sys_desc_bullets = LlC $ enumSimple physSystDescription_label 1 (short Doc.physSyst) physSystDescriptionListPhysys

physSystDescriptionListPhysys :: [Sentence]
physSystDescriptionListPhysys1 :: Sentence
physSystDescriptionListPhysys2 :: Sentence

physSystDescriptionListPhysys = [physSystDescriptionListPhysys1, physSystDescriptionListPhysys2]

physSystDescriptionListPhysys1 = foldlSent_ [at_start' itslPrpty, S "convention is noted by j. The end",
  plural itslPrpty, S "are usually not of", phrase interest `sC`
  S "therefore use the", plural itslPrpty, S "from" +:+.
  (E $ real_interval index $ Bounded (Inc,1) (Inc,sy numbSlices -1))]

physSystDescriptionListPhysys2 = foldlSent_ [at_start slice, plural property +:+ S "convention is noted by" +:+.
  (ch index)]

phys_sys_desc_p2 = foldlSP [S "A", phrase fbd, S "of the", 
  plural force, S "acting on the", phrase slice, 
  S "is displayed in", makeRef2S fig_forceacting]

fig_indexconv :: LabelledContent
fig_indexconv = llcc (makeFigRef "IndexConvention") $ 
  fig (foldlSent_ [S "Index convention for numbering",
  phrase slice `sAnd` phrase intrslce,
  phrase force, plural variable]) (resourcePath ++ "IndexConvention.png")

fig_forceacting :: LabelledContent
fig_forceacting = llcc (makeFigRef "ForceDiagram") $
  fig (at_start' force +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

-- SECTION 4.1.3 --
goal_stmt = goalStmtF (map (\(x, y) -> x `ofThe` y) [
  (S "geometry", S "water" +:+ phrase table_),
  (S "geometry", S "layers composing the plane of a" +:+ phrase slope),
  (plural mtrlPrpty, S "layers")
  ]) [goals_list]

goals_list = LlC $ enumSimple goalStmt_label 1 (short goalStmt) sspGoals

-- SECTION 4.2 --

-- SECTION 4.2.1 --
-- Assumptions is automatically generated

-- SECTION 4.2.2 --
-- TModels is automatically generated

-- SECTION 4.2.3 --
-- General Definitions is automatically generated

-- SECTION 4.2.4 --
-- Data Definitions is automatically generated
--FIXME: derivations should be with the appropriate DDef

-- SECTION 4.2.5 --
-- Instance Models is automatically generated
--FIXME: derivations should be with the appropriate IMod

-- SECTION 4.2.6 --
-- Data Constraints is automatically generated

{-
{-input data-}
noTypicalVal, vertConvention :: Sentence
noTypicalVal   = short notApp
vertConvention = S "Consecutive vertexes have increasing x" +:+.
  plural value +:+ S "The start and end vertices of all layers" +:+
  S "go to the same x" +:+. plural value --Monotonicly increasing?

verticesConst :: Sentence -> [Sentence]
verticesConst vertexType = [vertVar vertexType, vertConvention,
  noTypicalVal, noTypicalVal, noTypicalVal]

waterVert, slipVert, slopeVert :: [Sentence]
waterVert = verticesConst $ S "water" +:+ phrase table_
slipVert  = verticesConst $ phrase slip
slopeVert = verticesConst $ phrase slope
-}
{-input and output tables-}
data_constraint_Table2, data_constraint_Table3 :: LabelledContent
data_constraint_Table2 = inDataConstTbl sspInputs --FIXME: issue #295
data_constraint_Table3 = outDataConstTbl sspOutputs

-- SECTION 5 --

-- SECTION 5.1 --
funcReqList :: [Contents]
funcReqList = (mkEnumSimpleD sspRequirements) ++
  [LlC sspInputDataTable]

-- SECTION 5.2 --

-- SECTION 6 --
--Likely Changes is automatically generated

-- SECTION 7 --
aux_cons = valsOfAuxConstantsF ssa []

-- References --
-- automatically generated
