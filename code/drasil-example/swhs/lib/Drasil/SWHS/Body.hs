{-# LANGUAGE PostfixOperators #-}
module Drasil.SWHS.Body where

import Control.Lens ((^.))

import Language.Drasil hiding (organization, section, variable)
import Drasil.SRSDocument
import Drasil.Generator (cdb)
import qualified Drasil.DocLang.SRS as SRS (inModel)
import Theory.Drasil (GenDefn, InstanceModel)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (SystemKind(Specification), mkSystem)

import Drasil.Metadata (inModel)
import Data.Drasil.Concepts.Documentation as Doc (assumption, column,
  condition, constraint, corSol, datum, document, environment,input_, model,
  output_, physical, physics, property, quantity, software, softwareSys,
  solution, sysCont, system, user, value, variable)
import Data.Drasil.Concepts.Education (calculus, engineering)
import Data.Drasil.Concepts.Math (de, equation, ode, rightSide, unit_, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import qualified Data.Drasil.Concepts.Physics as CP (energy, mechEnergy, pressure)
import Data.Drasil.Concepts.Software (program, softwarecon)
import Data.Drasil.Concepts.Thermodynamics (enerSrc, heatTrans, htFlux,
  htTransTheo, lawConsEnergy, thermalAnalysis, thermalConduction, thermalEnergy,
  thermocon)
import Data.Drasil.Quantities.Math (surArea, surface, uNormalVect, area)
import Data.Drasil.Quantities.PhysicalProperties (vol)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (heatCapSpec, latentHeat)

import Data.Drasil.People (brooks, spencerSmith, thulasi)

import Drasil.SWHS.Assumptions (assumpPIS, assumptions)
import Drasil.SWHS.Changes (likelyChgs, unlikelyChgs)
import Drasil.SWHS.Concepts (acronymsFull, coil, con, phaseChangeMaterial,
  phsChgMtrl, sWHT, tank, tankPCM, transient, water)
import qualified Drasil.SWHS.DataDefs as SWHS (dataDefs)
import Drasil.SWHS.GenDefs (genDefs, htFluxWaterFromCoil, htFluxPCMFromWater)
import Drasil.SWHS.Goals (goals)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM,
  iMods, instModIntro)
import Drasil.SWHS.LabelledContent (labelledContent, figTank, sysCntxtFig)
import Drasil.SWHS.MetaConcepts (progName, progName')
import Drasil.SWHS.References (citations, uriReferences)
import Drasil.SWHS.Requirements (funcReqs, inReqDesc, nfRequirements,
  verifyEnergyOutput)
import Drasil.SWHS.TMods (tMods)
import Drasil.SWHS.Unitals (coilHTC, coilSA, consTol, constrained,
  htFluxC, htFluxP, inputs, inputConstraints, outputs, pcmE, pcmHTC, pcmSA,
  simTime, specParamValList, symbols, symbolsAll, tempC, tempPCM,
  tempW, thickness, watE)

si :: System
si = mkSystem
  progName' Specification [thulasi, brooks, spencerSmith]
  [purp] [] [scope] [motivation]
  symbols
  tMods genDefs SWHS.dataDefs iMods
  []
  inputs outputs constrained specParamValList
  symbMap

purp :: Sentence
purp = foldlSent_ [S "investigate the effect" `S.of_` S "employing",
  short phsChgMtrl, S "within a", phrase sWHT]

motivation :: Sentence
motivation = foldlSent_ [S "the demand" `S.is` S "high for renewable",
  D.toSent (pluralNP (enerSrc `and_PS` energy)), S "storage technology"]

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  materialProprty :
  -- CIs
  map nw [progName', progName] ++ [nw phsChgMtrl] ++
  map nw mathcon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  thermocon ++ softwarecon ++ physicalcon ++ con ++ [CP.energy,
  CP.mechEnergy, CP.pressure] ++
  -- UnitalChunks
  map cw [surArea, area]

symbMap :: ChunkDB
symbMap = cdb symbolsAll ideaDicts conceptChunks [] SWHS.dataDefs insModel
  genDefs tMods concIns labelledContent allRefs citations

abbreviationsList :: [IdeaDict]
abbreviationsList =
  -- CIs
  nw progName : map nw acronymsFull ++
  -- DefinedQuantityDicts
  map nw symbols

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = externalLinkRef : uriReferences

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $ RefProg intro [
    TUnits,
    tsymb'' tSymbIntro $ TermExcept [uNormalVect],
    TAandA abbreviationsList],
  IntroSec $
    IntroProg (introStart +:+ introStartSWHS) (introEnd (plural progName') progName)
    [IPurpose $ purpDoc progName Verbose,
     IScope scope,
     IChar [] charsOfReader [],
     IOrgSec inModel (SRS.inModel [] []) orgDocEnd
    ],
  GSDSec $ GSDProg
    [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets progName]
    , UsrChars [userChars progName]
    , SystCons [] []
    ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg purp []
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
    FReqsSub inReqDesc [],
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

concIns :: [ConceptInstance]
concIns = goals ++ assumptions ++ likelyChgs ++ unlikelyChgs ++ funcReqs
  ++ nfRequirements

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

introStart :: Sentence
introStart = foldlSent [S "Due to", foldlList Comma List (map S
  ["increasing costs", "diminishing availability", "negative environmental impact"]) `S.of_`
  S "fossil fuels" `sC` S "the demand" `S.is` S "high for renewable",
  D.toSent (pluralNP (enerSrc `and_PS` energy)), S "storage technology"]

introStartSWHS :: Sentence
introStartSWHS = foldlSent [D.toSent $ atStartNP' $ progName ^. term, S "incorporating",
  phrase phsChgMtrl, sParen (short phsChgMtrl), S "use a renewable",
  phrase enerSrc `S.and_` S "provide a novel way of storing" +:+. phrase energy,
  atStart progName', S "improve over the traditional", plural progName,
  S "because of their smaller size. The smaller size" `S.is` S "possible because" `S.ofThe` S "ability" `S.of_`
  short phsChgMtrl, S "to store", phrase thermalEnergy, S "as", phrase latentHeat `sC`
  S "which allows higher", phrase thermalEnergy, S "storage capacity per",
  phrase unit_, S "weight"]

introEnd :: Sentence -> CI -> Sentence
introEnd progSent pro = foldlSent_ [(progSent !.), S "The developed",
  phrase program, S "will be referred to as", titleize pro, sParen (short pro),
  S "based on the original" `sC` S "manually created version of" +:+
  namedRef externalLinkRef (S "SWHS")]
  -- SSP has same style sentence here

externalLinkRef :: Reference
externalLinkRef = makeURI "SWHS_SRSLink"
  "https://github.com/smiths/swhs/tree/master"
  (shortname' $ S "SWHS_SRSLink")

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose

--How to italicize words in sentence?
--How to cite?

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

scope :: Sentence
scope = foldlSent_ [phrase thermalAnalysis `S.of_` S "a single" +:+. phrase tankPCM,
  S "This entire", phrase document `S.is` S "written assuming that the substances inside the",
  phrase sWHT `S.are` (phrase water `S.and_` short phsChgMtrl)]

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
  S "mechanical", phrase engineering]

charReaderDE :: Sentence
charReaderDE = plural de +:+ S "from level 1 and 2" +:+ phrase calculus

------------------------------------
-- 2.4 : Organization of Document --
------------------------------------
orgDocEnd :: Sentence
orgDocEnd = foldlSent_ [D.toSent (atStartNP' (the inModel)),
  S "to be solved" `S.are` S "referred to as" +:+.
  foldlList Comma List (map refS iMods), S "The", plural inModel,
  S "provide the", plural ode, sParen (short ode :+: S "s") `S.and_`
  S "algebraic", plural equation, S "that", phrase model,
  (D.toSent (phraseNP (the progName')) !.), short progName, S "solves these", short ode :+: S "s"]

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
sysCntxtDesc pro = foldlSP [refS sysCntxtFig, S "shows the" +:+.
  phrase sysCont, S "A circle represents an external entity outside the",
  phrase software `sC` D.toSent (phraseNP (the user)) +:+. S "in this case",
  S "A rectangle represents the", phrase softwareSys, S "itself" +:+.
  sParen (short pro), S "Arrows" `S.are` S "used to show the", plural datum,
  S "flow between the", D.toSent (phraseNP (system `andIts` environment))]

sysCntxtRespIntro :: CI -> Contents
sysCntxtRespIntro pro = foldlSPCol [short pro +:+. S "is mostly self-contained",
  S "The only external interaction" `S.is` S "through the", phrase user +:+.
  S "interface", S "responsibilities" `S.the_ofTheC` D.toSent (phraseNP (user `andThe`
  system)) `S.are` S "as follows"]

systContRespBullets :: CI -> Contents
systContRespBullets prog = UlC $ ulcc $ Enumeration $ bulletNested
  [titleize user +: S "Responsibilities", short prog +: S "Responsibilities"]
  $ map bulletFlat [userResp, swhsResp]

userResp :: [Sentence]
userResp = map foldlSent_ [
  [S "Provide the", phrase input_, plural datum `S.toThe`
    phrase system `sC` S "ensuring no errors" `S.inThe` plural datum, S "entry"],
  [S "Take care that consistent", plural unit_ `S.are` S "used for",
    phrase input_, plural variable]
  ]

swhsResp :: [Sentence]
swhsResp = map foldlSent_ [
  [S "Detect", plural datum, S "type mismatch" `sC` S "such as a string" `S.of_`
    S "characters instead" `S.ofA` S "floating point number"],
  [S "Determine if the", plural input_, S "satisfy the required",
    D.toSent (phraseNP (physical `and_` software)), plural constraint],
  [S "Calculate the required", plural output_]
  ]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userChars :: CI -> Contents
userChars pro = foldlSP [S "The end", phrase user `S.of_` short pro,
  S "should have an understanding" `S.of_` S "undergraduate Level 1 Calculus" `S.and_`
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

-- Introduction of Problem Description section derived from purp

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

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

goalInputs :: [Sentence]
goalInputs = [D.toSent (phraseNP (the tempC)),
  S "the initial" +:+ plural condition +:+ S "for the" +:+ D.toSent (phraseNP (tempW `andThe` tempPCM)),
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
dataContMid = foldlSent [D.toSent (atStartNP (the column)) `S.for` D.toSent (pluralNP (combineNINI software
  constraint)), S "restricts the range" `S.of_` plural input_,
  S "to reasonable", plural value]

dataContFooter :: Sentence
dataContFooter = foldlSent_ $ map foldlSent [

  [sParen (S "*"), S "These", plural quantity, S "cannot be equal to zero" `sC`
  S "or there will be a divide by zero in the", phrase model],

  [sParen (S "+"), S "These", plural quantity, S "cannot be zero" `sC`
  S "or there would be freezing", sParen (refS assumpPIS)],

  [sParen (S "++"), D.toSent (atStartNP' (NP.the (constraint `onThePS` surArea))),
  S "are calculated by considering the", phrase surArea, S "to", phrase vol +:+.
  S "ratio", D.toSent (atStartNP (the assumption)) `S.is` S "that the lowest ratio is 1" `S.and_`
  S "the highest possible" `S.is` eS (exactDbl 2 $/ sy thickness) `sC` S "where", ch thickness,
  S "is the thickness of a" +:+. (Quote (S "sheet") `S.of_` short phsChgMtrl),
  S "A thin sheet has the greatest", phrase surArea, S "to", phrase vol, S "ratio"],

  [sParen (S "**"), D.toSent (atStartNP (the constraint)), S "on the maximum", phrase time,
  S "at the end" `S.ofThe` S "simulation" `S.is` S "the total number of seconds" `S.in_` S "one day"]

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
                   htFluxWaterFromCoil htFluxPCMFromWater surface heatTrans,
  propCorSolDeriv2,
  propCorSolDeriv3 pcmE energy phsChgMtrl water,
  propCorSolDeriv4,
  propCorSolDeriv5 equation progName rightSide]

propCorSolDeriv1 :: (NamedIdea b, NamedIdea h) => ConceptChunk -> b -> UnitalChunk ->
  ConceptChunk -> CI -> GenDefn -> GenDefn -> h -> ConceptChunk -> Contents
propCorSolDeriv1 lce ewat en co pcmat g1hfc g2hfp su ht =
  foldlSPCol [D.toSent (atStartNP (a_ corSol)), S "must exhibit" +:+.
  D.toSent (phraseNP (the lce)), S "This means that", D.toSent (phraseNP (the ewat)),
  S "should equal the difference between the total", phrase en,
  phrase input_, S "from", D.toSent (phraseNP (the co `NP.andThe`
  combineNINI en output_)), S "to the" +:+. short pcmat,
  S "This can be shown as an", phrase equation, S "by taking",
  refS g1hfc `S.and_` refS g2hfp `sC`
  S "multiplying each by their respective", phrase su,
  S "area of", phrase ht `sC` S "and integrating each",
  S "over the", phrase simTime `sC` S "as follows"]

propCorSolDeriv2 :: Contents
propCorSolDeriv2 = unlbldExpr
  (sy watE $= defint (eqSymb time) (exactDbl 0) (sy time)
  (sy coilHTC $* sy coilSA $* (sy tempC $- apply1 tempW time))
  $- defint (eqSymb time) (exactDbl 0) (sy time)
  (sy pcmHTC $* sy pcmSA $* (apply1 tempW time $-
  apply1 tempPCM time)))

propCorSolDeriv3 :: NamedIdea a => a -> UnitalChunk -> CI -> ConceptChunk -> Contents
propCorSolDeriv3 epcm en pcmat wa =
  foldlSP_ [S "In addition, the", phrase epcm, S "should equal the",
  phrase en, phrase input_ `S.toThe` short pcmat,
  S "from the" +:+. phrase wa, S "This can be expressed as"]

propCorSolDeriv4 :: Contents
propCorSolDeriv4 = unlbldExpr
  (sy pcmE $= defint (eqSymb time) (exactDbl 0) (sy time)
  (sy pcmHTC $* sy pcmSA $* (apply1 tempW time $-
  apply1 tempPCM time)))

propCorSolDeriv5 :: ConceptChunk -> CI -> CI -> Contents
propCorSolDeriv5 eq pro rs = foldlSP [titleize' eq, S "(FIXME: Equation 7)"
  `S.and_` S "(FIXME: Equation 8) can be used as", Quote (S "sanity") +:+
  S "checks to gain confidence in any", phrase solution,
  S "computed by" +:+. short pro, S "The relative",
  S "error between the results computed by", short pro `S.and_`
  S "the results calculated from the", short rs, S "of these",
  plural eq, S "should be less than", ch consTol, refS verifyEnergyOutput]

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
------------------------------------------------
-- Section 8 : Specification Parameter Values --
------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------
