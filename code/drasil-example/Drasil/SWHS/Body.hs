module Drasil.SWHS.Body where

import Language.Drasil hiding (Symbol(..), organization, section)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB,
  SystemInformation(SI), cdb, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, InstanceModel, TheoryModel)
import Utils.Drasil

import Control.Lens ((^.))

import Drasil.DocLang (AuxConstntSec (AuxConsProg), DocSection (..),
  Field(..), Fields, LFunc(TermExcept), Literature(Doc', Lit), IntroSec(IntroProg),
  IntroSub(IChar, IOrgSec, IPurpose, IScope), RefSec (RefProg), 
  RefTab (TAandA, TUnits), TSIntro(SymbConvention, SymbOrder, TSPurpose, VectorUnits),
  ReqrmntSec(..), ReqsSub(..), SRSDecl, SSDSub(..), SolChSpec (SCSProg),
  SSDSec(..), InclUnits(..), DerivationDisplay(..), SCSSub(..), Verbosity(..),
  TraceabilitySec(TraceabilityProg), GSDSec(..), GSDSub(..),
  ProblemDescription(PDProg), PDSub(..), intro, mkDoc, tsymb'', traceMatStandard)
import qualified Drasil.DocLang.SRS as SRS (inModel)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (inModel, thModel)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, column, condition,
  constraint, content, corSol, datum, definition, document, environment, goalStmt,
  information, input_, model, organization, output_, physical, physics, problem,
  property, purpose, quantity, reference, software, softwareSys, solution,
  srsDomains, sysCont, system, user, value, variable, doccon, doccon')
import Data.Drasil.Concepts.Education (calculus, educon, engineering)
import Data.Drasil.Concepts.Math (de, equation, ode, unit_, mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import Data.Drasil.Concepts.Physics (physicCon)
import Data.Drasil.Concepts.Software (program, softwarecon, correctness,
  understandability, reusability, maintainability, verifiability)
import Data.Drasil.Concepts.Thermodynamics (enerSrc, heatTrans, htFlux,
  htTransTheo, lawConsEnergy, thermalAnalysis, thermalConduction, thermalEnergy,
  thermocon)
import Data.Drasil.Quantities.Math (surArea, surface, uNormalVect)
import Data.Drasil.Quantities.PhysicalProperties (vol)
import Data.Drasil.Quantities.Physics (energy, time, physicscon)
import Data.Drasil.Quantities.Thermodynamics (heatCapSpec, latentHeat)
import Data.Drasil.Software.Products (sciCompS, prodtcon)

import Data.Drasil.People (brooks, spencerSmith, thulasi)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt,
  fundamentals, derived, m_2, m_3)

import Drasil.SWHS.Assumptions (assumpPIS, assumptions)
import Drasil.SWHS.Changes (likelyChgs, unlikelyChgs)
import Drasil.SWHS.Concepts (acronymsFull, coil, con, phaseChangeMaterial,
  phsChgMtrl, progName, rightSide, sWHT, swhsPCM, tank, tankPCM, transient, water)
import Drasil.SWHS.DataDefs (ddHtFluxC, ddHtFluxP, qDefs)
import qualified Drasil.SWHS.DataDefs as SWHS (dataDefs)
import Drasil.SWHS.GenDefs (genDefs)
import Drasil.SWHS.Goals (goals)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM,
  iMods, instModIntro)
import Drasil.SWHS.References (parnas1972, parnasClements1984, citations)
import Drasil.SWHS.Requirements (funcReqs, inputInitQuantsTable, nfRequirements,
  verifyEnergyOutput)
import Drasil.SWHS.TMods (consThermE, latentHtE, sensHtE)
import Drasil.SWHS.Unitals (absTol, coilHTC, coilSA, consTol, constrained,
  htFluxC, htFluxP, inputs, inputConstraints, outputs, pcmE, pcmHTC, pcmSA,
  relTol, simTime, specParamValList, symbols, symbolsAll, tempC, tempPCM,
  tempW, thickness, unitalChuncks, watE)

-------------------------------------------------------------------------------

srs :: Document
srs = mkDoc mkSRS for si

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

resourcePath :: String
resourcePath = "../../../datafiles/SWHS/"

units :: [UnitDefn]
units = map unitWrapper [metre, kilogram, second] ++ 
  map unitWrapper [centigrade, joule, watt]
--Will there be a table of contents?

si :: SystemInformation
si = SI {
  _sys = swhsPCM,
  _kind = Doc.srs, 
  _authors = [thulasi, brooks, spencerSmith],
  _quants = symbols,
  _concepts = [] :: [DefinedQuantityDict],
  _definitions = qDefs,
  _datadefs = SWHS.dataDefs,
  _inputs = inputs,
  _outputs = map qw outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = constrained,
  _constants = specParamValList,
  _sysinfodb = symbMap,
  _usedinfodb = usedDB,
   refdb = refDB
}

symbMap :: ChunkDB
symbMap = cdb (qw heatEInPCM : symbolsAll) -- heatEInPCM ?
  (nw heatEInPCM : map nw symbols ++ map nw acronymsFull
  ++ map nw thermocon ++ map nw units ++ map nw [m_2, m_3] ++ map nw [absTol, relTol]
  ++ map nw physicscon ++ map nw doccon ++ map nw softwarecon ++ map nw doccon' ++ map nw con
  ++ map nw prodtcon ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw specParamValList
  ++ map nw fundamentals ++ map nw educon ++ map nw derived ++ map nw physicalcon ++ map nw unitalChuncks
  ++ [nw swhsPCM, nw algorithm] ++ map nw compcon ++ [nw materialProprty])
  (cw heatEInPCM : map cw symbols ++ srsDomains) -- FIXME: heatEInPCM?
  (units ++ [m_2, m_3]) SWHS.dataDefs insModel genDefs theory concIns section labCon

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols ++ map nw acronymsFull)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] []

refDB :: ReferenceDB
refDB = rdb citations concIns

mkSRS :: SRSDecl
mkSRS = [RefSec $ RefProg intro [
    TUnits,
    tsymb'' tSymbIntro $ TermExcept [uNormalVect],
    TAandA],
  IntroSec $
    IntroProg (introStart +:+ introStartSWHS) (introEnd (plural swhsPCM) progName)
    [IPurpose $ purpDoc (phrase swhsPCM) progName,
     IScope scope,
     IChar [] charsOfReader [],
     IOrgSec orgDocIntro inModel (SRS.inModel [] []) orgDocEnd
    ],
  GSDSec $ GSDProg2 
    [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets]
    , UsrChars [userChars progName]
    , SystCons [] []
    ],
  SSDSec $
    SSDProg 
      [ SSDProblem $ PDProg probDescIntro []
        [ TermsAndDefs Nothing terms
        , PhySysDesc progName physSystParts figTank []
        , Goals goalInputs]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints dataConTail inputConstraints
        , CorrSolnPpties outputConstraints propsDeriv
        ]
      ],
  ReqrmntSec $ ReqsProg [
    FReqsSub [inputInitQuantsTable],
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg progName specParamValList,
  Bibliography]

tSymbIntro :: [TSIntro]
tSymbIntro = [TSPurpose, SymbConvention
  [Lit (nw heatTrans), Doc' (nw progName)], SymbOrder, VectorUnits]

insModel :: [InstanceModel]
insModel = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

theory :: [TheoryModel]
theory = [consThermE, sensHtE, latentHtE]

concIns :: [ConceptInstance]
concIns = goals ++ assumptions ++ likelyChgs ++ unlikelyChgs ++ funcReqs
  ++ nfRequirements

labCon :: [LabelledContent]
labCon = [inputInitQuantsTable]

section :: [Section]
section = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

priorityNFReqs :: [ConceptChunk]
priorityNFReqs = [correctness, verifiability, understandability, reusability,
  maintainability]
-- It is sometimes hard to remember to add new sections both here and above.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

introStart :: Sentence
introStart = foldlSent [S "Due to", foldlList Comma List (map S
  ["increasing costs", "diminishing availability", "negative environmental impact"]) `sOf`
  S "fossil fuels" `sC` S "the demand is high for renewable", plural enerSrc `sAnd`
  phrase energy, S "storage technology"]

introStartSWHS :: Sentence
introStartSWHS = foldlSent [swhsPCM ^. defn, sParen (short phsChgMtrl),
  S "use a renewable", phrase enerSrc `sAnd` S "provide a novel way of storing" +:+.
  phrase energy, atStart swhsPCM, S "improve over the traditional", plural progName,
  S "because of their smaller size. The smaller size is possible because of the ability" `sOf`
  short phsChgMtrl, S "to store", phrase thermalEnergy, S "as", phrase latentHeat `sC`
  S "which allows higher", phrase thermalEnergy, S "storage capacity per",
  phrase unit_, S "weight"]

introEnd :: Sentence -> CI -> Sentence
introEnd progSent pro = foldlSent_ [EmptyS +:+. progSent, S "The developed",
  phrase program, S "will be referred to as", titleize pro, sParen (short pro)]
  -- SSP has same style sentence here

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

purpDoc :: Sentence -> CI -> Sentence
purpDoc spSent pro = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  spSent, S "The", plural goalStmt `sAnd` plural thModel,
  S "used in the", short pro, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption `sAnd` S "unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short Doc.srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]


-- Besides program name, these two paragraphs are general, mostly repeated
-- between examples, and can be abstracted out.

--How to italicize words in sentence?
--How to cite?

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

scope :: Sentence
scope = foldlSent_ [phrase thermalAnalysis `sOf` S "a single" +:+. phrase tankPCM,
  S "This entire", phrase document `sIs` S "written assuming that the substances inside the",
  phrase sWHT `sAre` phrase water `sAnd` short phsChgMtrl]

-- There is a similar paragraph in each example, but there's a lot of specific
-- info here. Would need to abstract out the object of analysis (i.e. solar
-- water heating tank rating PCM, 2D slope composed of homogeneous soil
-- layers, glass slab and blast, or 2D bodies acted on by forces) and also
-- abstract out the overall goal of the program (i.e. predict the temperature
-- and energy histories for the water and PCM, simulate how 2D rigid bodies
-- interact with each other, predict whether the glass slab is safe to use or
-- not, etc.). If that is done, then this paragraph can also be abstracted out.

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

charsOfReader :: [Sentence]
charsOfReader = [charReaderHTT, charReaderDE]

charReaderHTT :: Sentence
charReaderHTT = foldlSent_ [phrase htTransTheo, S "from level 3 or 4",
  S "mechanical",  phrase engineering]

charReaderDE :: Sentence
charReaderDE = plural de +:+ S "from level 1 and 2" +:+ phrase calculus

------------------------------------
-- 2.4 : Organization of Document --
------------------------------------

orgDocIntro :: Sentence
orgDocIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short Doc.srs,
  S "for", phrase sciCompS, S "proposed by", makeCiteS parnas1972 `sAnd` 
  makeCiteS parnasClements1984]

orgDocEnd :: Sentence
orgDocEnd = foldlSent_ [S "The", plural inModel, 
  S "to be solved are referred to as" +:+. 
  foldlList Comma List (map makeRef2S iMods), S "The", plural inModel,
  S "provide the", phrase ode, sParen (short ode :+: S "s") `sAnd` 
  S "algebraic", plural equation, S "that", phrase model, S "the" +:+. 
  phrase swhsPCM, short progName, S "solves these", short ode :+: S "s"]

-- This paragraph is mostly general (besides program name and number of IMs),
-- but there are some differences between the examples that I'm not sure how to
-- account for. Specifically, the glass example references a Volere paper that
-- is not used for the other examples. Besides that, this paragraph could
-- probably be abstracted out with some changes (i.e. the other examples don't
-- include the last sentence, so we might not need to know the number of IMs
-- after all if we just leave that sentence out)

-- IM1 to IM4 : reference later

-- how to cite/reference?

-- If all SRS have the same basic layout, is it possible to automate
-- the sectioning? This would also improve the tediousness of declaring
-- LayoutObjs

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

--------------------------
-- 3.1 : System Context --
--------------------------

sysCntxtDesc :: CI -> Contents
sysCntxtDesc pro = foldlSP [makeRef2S sysCntxtFig, S "shows the" +:+.
  phrase sysCont, S "A circle represents an external entity outside the",
  phrase software `sC` S "the", phrase user +:+. S "in this case",
  S "A rectangle represents the", phrase softwareSys, S "itself" +:+.
  sParen (short pro), S "Arrows are used to show the", plural datum,
  S "flow between the", phrase system `sAnd` S "its", phrase environment]

sysCntxtFig :: LabelledContent
sysCntxtFig = llcc (makeFigRef "SysCon") $ fig (foldlSent_
  [makeRef2S sysCntxtFig +: EmptyS, titleize sysCont])
  $ resourcePath ++ "SystemContextFigure.png"

sysCntxtRespIntro :: CI -> Contents
sysCntxtRespIntro pro = foldlSPCol [short pro +:+. S "is mostly self-contained",
  S "The only external interaction is through the", phrase user +:+.
  S "interface", S "responsibilities" `ofThe'` phrase user `andThe`
  phrase system `sAre` S "as follows"]

systContRespBullets :: Contents
systContRespBullets = UlC $ ulcc $ Enumeration $ bulletNested
  [titleize user +: S "Responsibilities", short progName +: S "Responsibilities"]
  $ map bulletFlat [userResp, swhsResp]

userResp :: [Sentence]
userResp = map foldlSent_ [
  [S "Provide the", phrase input_, plural datum `toThe`
    phrase system `sC` S "ensuring no errors in the", plural datum, S "entry"],
  [S "Take care that consistent", plural unit_, S "are used for",
    phrase input_, plural variable]
  ]

swhsResp :: [Sentence]
swhsResp = map foldlSent_ [
  [S "Detect", plural datum, S "type mismatch, such as a string" `sOf`
    S "characters instead of a floating point number"],
  [S "Determine if the", plural input_, S "satisfy the required",
    phrase physical `sAnd` phrase software, plural constraint],
  [S "Calculate the required", plural output_]
  ]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userChars :: CI -> Contents
userChars pro = foldlSP [S "The end", phrase user `sOf` short pro,
  S "should have an understanding of undergraduate Level 1 Calculus" `sAnd`
  titleize Doc.physics]

-- Some of these course names are repeated between examples, could potentially
-- be abstracted out.

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-------------------------------
-- 4.1 : Problem Description --
-------------------------------
probDescIntro :: Sentence
probDescIntro = foldlSent_ [S "investigate the effect" `sOf` S "employing",
  short phsChgMtrl, S "within a", phrase sWHT]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

terms :: [ConceptChunk]
terms = map cw [htFlux, phaseChangeMaterial, cw heatCapSpec, thermalConduction, transient]

-- Included heat flux and specific heat in NamedChunks even though they are
-- already in SWHSUnits

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

physSystParts :: [Sentence]
physSystParts = map foldlSent_ [physSyst1 tank water, physSyst2 coil tank htFluxC,
  [short phsChgMtrl, S "suspended in" +:+. phrase tank,
  sParen (ch htFluxP +:+ S "represents the" +:+. phrase htFluxP)]]

physSyst1 :: ConceptChunk -> ConceptChunk -> [Sentence]
physSyst1 ta wa = [atStart ta, S "containing" +:+. phrase wa]

physSyst2 :: ConceptChunk -> ConceptChunk -> UnitalChunk -> [Sentence]
physSyst2 co ta hfc = [atStart co, S "at bottom of" +:+. phrase ta,
  sParen (ch hfc +:+ S "represents the" +:+. phrase hfc)]

-- Structure of list would be same between examples but content is completely
-- different

figTank :: LabelledContent
figTank = llcc (makeFigRef "Tank") $ fig (
  foldlSent_ [atStart sWHT `sC` S "with", phrase htFluxC `sOf`
  ch htFluxC `sAnd` phrase htFluxP `sOf` ch htFluxP])
  $ resourcePath ++ "Tank.png"

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

goalInputs :: [Sentence]
goalInputs  = [S "the" +:+ phrase tempC,
  S "the initial" +:+ plural condition +:+ S "for the" +:+ phrase tempW `andThe` phrase tempPCM,
  S "the material" +:+ plural property]

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be
-- abstracted out if this paragraph were to be abstracted out.

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

-- SECTION 4.2.3 --
{--- General Definitions is automatically generated in solChSpecF
s4_2_3_genDefs :: [Contents]
s4_2_3_genDefs = map reldefn swhsRC

s4_2_3_deriv :: [Contents]
s4_2_3_deriv = [s4_2_3_deriv_1 rOfChng temp,
  s4_2_3_deriv_2 consThermE vol,
  s4_2_3_deriv_3,
  s4_2_3_deriv_4 gaussDiv surface vol thFluxVect uNormalVect unit_,
  s4_2_3_deriv_5,
  s4_2_3_deriv_6 vol volHtGen,
  s4_2_3_deriv_7,
  s4_2_3_deriv_8 htFluxIn htFluxOut inSA outSA density heatCapSpec
    temp vol assumption assump3 assump4 assump5 assump6,
  s4_2_3_deriv_9,
  s4_2_3_deriv_10 density mass vol,
  s4_2_3_deriv_11]-}

-- General Definitions is automatically generated 

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------
----------------------------
-- 4.2.6 Data Constraints --
----------------------------
-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.
dataConTail :: Sentence
dataConTail = dataContMid +:+ dataContFooter

dataContMid :: Sentence
dataContMid = foldlSent [S "The", phrase column, S "for", phrase software,
  plural constraint, S "restricts the range" `sOf`  plural input_,
  S "to reasonable", plural value]

dataContFooter :: Sentence
dataContFooter = foldlSent_ $ map foldlSent [

  [sParen (S "*"), S "These", plural quantity, S "cannot be equal to zero" `sC`
  S "or there will be a divide by zero in the", phrase model],

  [sParen (S "+"), S "These", plural quantity, S "cannot be zero" `sC`
  S "or there would be freezing", sParen (makeRef2S assumpPIS)],

  [sParen (S "++"), S "The", plural constraint, S "on the", phrase surArea,
  S "are calculated by considering the", phrase surArea, S "to", phrase vol +:+.
  S "ratio", S "The", phrase assumption, S "is that the lowest ratio is 1" `sAnd`
  S "the highest possible is", E (2 / sy thickness) `sC` S "where", ch thickness,
  S "is the thickness of a" +:+. (Quote (S "sheet") `sOf` short phsChgMtrl),
  S "A thin sheet has the greatest", phrase surArea, S "to", phrase vol, S "ratio"],

  [sParen (S "**"), S "The", phrase constraint, S "on the maximum", phrase time,
  S "at the end of the simulation is the total number of seconds in one day"]
  
  ]
------------------------------
-- Data Constraint: Table 1 --
------------------------------

------------------------------
-- Data Constraint: Table 2 --
------------------------------

------------------------------
-- Data Constraint: Table 3 --
------------------------------

outputConstraints :: [ConstrConcept]
outputConstraints = [tempW, tempPCM, watE, pcmE] --FIXME: add "(by A11)" in Physical Constraints of `tempW` and `tempPCM`?

-- Other Notes:
---- Will there be a way to have asterisks for certain pieces of the table?

----------------------------------------------
-- 4.2.7 : Properties of A Correct Solution --
----------------------------------------------
{-Properties of a Correct Solution-}

propsDeriv :: [Contents]
propsDeriv = [
  propCorSolDeriv1 lawConsEnergy watE energy coil phsChgMtrl
                   ddHtFluxC ddHtFluxP surface heatTrans,
  propCorSolDeriv2,
  propCorSolDeriv3 pcmE energy phsChgMtrl water,
  propCorSolDeriv4,
  propCorSolDeriv5 equation progName rightSide]

propCorSolDeriv1 :: (NamedIdea b, NamedIdea h) => ConceptChunk -> b -> UnitalChunk -> ConceptChunk ->
  CI -> DataDefinition -> DataDefinition -> h -> ConceptChunk -> Contents
propCorSolDeriv1 lce ewat en co pcmat d1hfc d2hfp su ht  =
  foldlSPCol [S "A", phrase corSol, S "must exhibit the" +:+.
  phrase lce, S "This means that the", phrase ewat,
  S "should equal the difference between the total", phrase en,
  phrase input_, S "from the", phrase co `sAnd` S "the",
  phrase en, phrase output_, S "to the" +:+. short pcmat,
  S "This can be shown as an", phrase equation, S "by taking",
  makeRef2S d1hfc `sAnd` makeRef2S d2hfp `sC`
  S "multiplying each by their respective", phrase su,
  S "area of", phrase ht `sC` S "and integrating each",
  S "over the", phrase simTime `sC` S "as follows"]

propCorSolDeriv2 :: Contents
propCorSolDeriv2 = eqUnR'
  (sy watE $= defint (eqSymb time) 0 (sy time)
  (sy coilHTC * sy coilSA * (sy tempC - apply1 tempW time))
  - defint (eqSymb time) 0 (sy time)
  (sy pcmHTC * sy pcmSA * (apply1 tempW time -
  apply1 tempPCM time)))

propCorSolDeriv3 :: NamedIdea a => a -> UnitalChunk -> CI -> ConceptChunk -> Contents
propCorSolDeriv3 epcm en pcmat wa =
  foldlSP_ [S "In addition, the", phrase epcm, S "should equal the",
  phrase en, phrase input_, S "to the", short pcmat,
  S "from the" +:+. phrase wa, S "This can be expressed as"]

propCorSolDeriv4 :: Contents
propCorSolDeriv4 = eqUnR'
  (sy pcmE $= defint (eqSymb time) 0 (sy time)
  (sy pcmHTC * sy pcmSA * (apply1 tempW time - 
  apply1 tempPCM time)))

propCorSolDeriv5 :: ConceptChunk -> CI -> CI -> Contents
propCorSolDeriv5 eq pro rs = foldlSP [titleize' eq, S "(FIXME: Equation 7)" 
  `sAnd` S "(FIXME: Equation 8) can be used as", Quote (S "sanity") +:+
  S "checks to gain confidence in any", phrase solution,
  S "computed by" +:+. short pro, S "The relative",
  S "error between the results computed by", short pro `sAnd`
  S "the results calculated from the", short rs, S "of these",
  plural eq, S "should be less than", ch consTol, makeRef2S verifyEnergyOutput]

-- Remember to insert references in above derivation when available

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

--------------------------------
-- Section 6b : UNLIKELY CHANGES --
--------------------------------

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

------------------------
-- Traceabilty Graphs --
------------------------
-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------
