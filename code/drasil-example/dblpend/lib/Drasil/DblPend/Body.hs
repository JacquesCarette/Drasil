{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.Body where

import Control.Lens ((^.))

import Drasil.Metadata (inModel)
import Language.Drasil hiding (organization, section)
import Theory.Drasil (TheoryModel, output)
import Drasil.SRSDocument
import Database.Drasil.ChunkDB (cdb)
import qualified Drasil.DocLang.SRS as SRS

import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.People (dong)
import Data.Drasil.Concepts.Computation (inDatum)
import qualified Data.Drasil.Concepts.Documentation as Doc (physics, variable)
import Data.Drasil.Concepts.Documentation (assumption, condition, endUser,
  environment, datum, input_, interface, output_, problem, product_,
  physical, sysCont, software, softwareConstraint, softwareSys,
  system, user, analysis)
import Data.Drasil.Concepts.Education (highSchoolPhysics, highSchoolCalculus, calculus, undergraduate)
import Data.Drasil.Concepts.Math (cartesian, ode, mathcon', graph)
import Data.Drasil.Concepts.Physics (gravity, physicCon, physicCon', pendulum, twoD, motion)
import Data.Drasil.Concepts.PhysicalProperties (mass, physicalcon)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Theories.Physics (newtonSL, accelerationTM, velocityTM)

import Drasil.DblPend.Figures (figMotion, sysCtxFig1)
import Drasil.DblPend.Assumptions (assumpDouble)
import Drasil.DblPend.Concepts (rod, concepts, pendMotion, firstRod, secondRod, firstObject, secondObject)
import Drasil.DblPend.Goals (goals, goalsInputs)
import Drasil.DblPend.DataDefs (dataDefs)
import Drasil.DblPend.IMods (iMods)
import Drasil.DblPend.GenDefs (genDefns)
import Drasil.DblPend.MetaConcepts (progName)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, symbols, inputs, outputs,
  inConstraints, outConstraints, acronyms, pendDisAngle, constants)
import Drasil.DblPend.Requirements (funcReqs, nonFuncReqs)
import Drasil.DblPend.References (citations)
import Data.Drasil.ExternalLibraries.ODELibraries (scipyODESymbols,
  osloSymbols, apacheODESymbols, odeintSymbols, arrayVecDepVar)
import Language.Drasil.Code (quantvar)
import Drasil.DblPend.ODEs (dblPenODEInfo)

import System.Drasil (SystemKind(Specification), mkSystem)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [TableOfContents, -- This creates the Table of Contents
  RefSec $      --This creates the Reference section of the SRS
    RefProg intro      -- This add the introduction blob to the reference section
      [ TUnits         -- Adds table of unit section with a table frame
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits] -- Adds table of symbol section with a table frame
      -- introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
      , TAandA abbreviationsList         -- Add table of abbreviation and acronym section
      ],
  IntroSec $
    IntroProg (justification progName) (phrase progName)
      [IPurpose $ purpDoc progName Verbose,
       IScope scope,
       IChar [] charsOfReader [],
       IOrgSec inModel (SRS.inModel [] []) EmptyS],
  GSDSec $ 
    GSDProg [
      SysCntxt [sysCtxIntro progName, LlC sysCtxFig1, sysCtxDesc, sysCtxList progName],
      UsrChars [userCharacteristicsIntro progName], 
      SystCons [] []],                            
  SSDSec $ 
    SSDProg
      [ SSDProblem $ PDProg purp []                -- This adds a is used to define the problem your system will solve
        [ TermsAndDefs Nothing terms               -- This is used to define the terms to be defined in terminology sub section
      , PhySysDesc progName physSystParts figMotion [] -- This defines the Physicalsystem sub-section, define the parts
                                                          -- of the system using physSysParts, figMotion is a function in figures for the image
      , Goals goalsInputs] -- This adds a goals section and goals input is defined for the preample of the goal.
      , SSDSolChSpec $ SCSProg --This creates the solution characteristics section with a preamble
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $ ReqsProg
    [ FReqsSub EmptyS []
    , NonFReqsSub
    ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg progName [], -- Adds Auxilliary constraint section
  Bibliography                -- Adds reference section
  ]

si :: System
si = mkSystem progName Specification [dong]
  [purp] [background] [scope] [motivation]
  symbolsAll
  tMods genDefns dataDefs iMods
  []
  inputs outputs inConstraints
  constants
  symbMap

purp :: Sentence
purp = foldlSent_ [S "predict the", phrase motion `S.ofA` S "double", phrase pendulum]

motivation :: Sentence
motivation = foldlSent_ [S "To simulate", phraseNP (motion `the_ofThe` pendulum),
  S "and exhibit its chaotic characteristics"]

background :: Sentence
background = foldlSent_ [phraseNP (a_ pendulum), S "consists" `S.of_` phrase mass, 
  S "attached to the end" `S.ofA` phrase rod `S.andIts` S "moving curve" `S.is`
  S "highly sensitive to initial conditions"]

symbolsAll :: [DefinedQuantityDict]
symbolsAll = symbols ++ scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols 
  ++ map dqdWr [listToArray $ quantvar pendDisAngle, arrayVecDepVar dblPenODEInfo]

ideaDicts :: [IdeaDict]
ideaDicts = 
  -- Actual IdeaDicts
  concepts ++
  -- CIs
  nw progName : map nw mathcon' ++ map nw physicCon'

abbreviationsList :: [IdeaDict]
abbreviationsList = 
  -- DefinedQuantityDict abbreviations
  map nw symbols ++
  -- Other acronyms/abbreviations
  nw progName : map nw acronyms

conceptChunks :: [ConceptChunk]
conceptChunks = physicCon ++ physicalcon

symbMap :: ChunkDB
symbMap = cdb (map (^. output) iMods ++ symbolsAll)
  ideaDicts conceptChunks ([] :: [UnitDefn])
  dataDefs iMods genDefns tMods concIns [] allRefs citations

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef]

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

concIns :: [ConceptInstance]
concIns = assumpDouble ++ goals ++ funcReqs ++ nonFuncReqs
-- ++ likelyChgs ++ unlikelyChgs

------------------------------
-- Section : INTRODUCTION --
------------------------------
justification :: CI -> Sentence
justification prog = foldlSent [ atStartNP (a_ pendulum), S "consists" `S.of_` phrase mass, 
                            S "attached to the end" `S.ofA` phrase rod `S.andIts` S "moving curve" `S.is`
                            (S "highly sensitive to initial conditions" !.), S "Therefore" `sC`
                            S "it is useful to have a", phrase program, S "to simulate", phraseNP (motion
                            `the_ofThe` pendulum), (S "to exhibit its chaotic characteristics" !.),
                            S "The document describes the program called", phrase prog,
                            S ", which is based on the original, manually created version of" +:+
                            namedRef externalLinkRef (S "Double Pendulum")]
                            
externalLinkRef :: Reference
externalLinkRef = makeURI "DblPendSRSLink" 
  "https://github.com/Zhang-Zhi-ZZ/CAS741Project/tree/master/Double%20Pendulum" 
  (shortname' $ S "DblPendSRSLink")                            
-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope :: Sentence
scope = foldlSent_ [phraseNP (NP.the (analysis `ofA` twoD)), 
  sParen (short twoD), phrase pendMotion, phrase problem,
                   S "with various initial conditions"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------
charsOfReader :: [Sentence]
charsOfReader = [phrase undergraduate +:+ S "level 2" +:+ phrase Doc.physics,
                 phrase undergraduate +:+ S "level 1" +:+ phrase calculus,
                 plural ode]

-------------------------------------
-- 2.4 : Organization of Documents --
-------------------------------------
-- Starting intro sentence of Organization of Documents automatically generated
-- in IOrg

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
-- Description of Genreal System automatically generated in GSDProg

--------------------------
-- 3.1 : System Context --
--------------------------
sysCtxIntro :: CI -> Contents
sysCtxIntro prog = foldlSP
  [refS sysCtxFig1, S "shows the" +:+. phrase sysCont,
   S "A circle represents an entity external" `S.toThe` phrase software
   `sC` phraseNP (the user), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", sParen (short prog) +:+. EmptyS,
   S "Arrows" `S.are` S "used to show the data flow between the", phraseNP (system
   `andIts` environment)]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol [S "The interaction between the", phraseNP (product_
   `andThe` user), S "is through an application programming" +:+.
   phrase interface, S "The responsibilities" `S.ofThe` phraseNP (user 
   `andThe` system), S "are as follows"]

sysCtxUsrResp :: CI -> [Sentence]
sysCtxUsrResp prog = [S "Provide initial" +:+ pluralNP (condition `ofThePS`
  physical) +:+ S "state" `S.ofThe` phrase motion +:+ S "and the" +:+ plural inDatum +:+ S "related to the" +:+
  phrase prog `sC` S "ensuring no errors in the" +:+
  plural datum +:+. S "entry",
  S "Ensure that consistent units" `S.are` S "used for" +:+. pluralNP (combineNINI input_ Doc.variable),
  S "Ensure required" +:+
  namedRef (SRS.assumpt ([]::[Contents]) ([]::[Section])) (phrase software +:+ plural assumption) +:+
  S "are appropriate for any particular" +:+
  phrase problem +:+ S "input to the" +:+. phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters" +:+
  phrase input_ +:+. (S "instead" `S.ofA` S "floating point number"),
  S "Determine if the" +:+ plural input_ +:+ S "satisfy the required" +:+.
  pluralNP (physical `and_` softwareConstraint),
  S "Calculate the required" +:+. plural output_, 
  S "Generate the required" +:+. plural graph]

sysCtxResp :: CI -> [Sentence]
sysCtxResp prog = [titleize user +:+ S "Responsibilities",
  short prog +:+ S "Responsibilities"]

sysCtxList :: CI -> Contents
sysCtxList prog = UlC $ ulcc $ Enumeration $ bulletNested (sysCtxResp prog) $
  map bulletFlat [sysCtxUsrResp prog, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------
userCharacteristicsIntro :: CI -> Contents
userCharacteristicsIntro prog = foldlSP
  [S "The", phrase endUser `S.of_` short prog,
   S "should have an understanding of", 
   phrase highSchoolPhysics `sC` phrase highSchoolCalculus `S.and_` plural ode]

-------------------------------
-- 3.3 : System Constraints  --
-------------------------------
-- System Constraints automatically generated in SystCons


--------------------------------------------
-- Section 4: Specific System Description --
--------------------------------------------
-- Description of Specific System automatically generated in SSDProg

-------------------------------
-- 4.1 : System Constraints  --
-------------------------------
-- Introduction of the Problem Description section derived from purp

---------------------------------
-- 4.1.1 Terminology and Definitions --
---------------------------------
terms :: [ConceptChunk]
terms = [gravity, cartesian]

-----------------------------------
-- 4.1.2 Physical System Description --
-----------------------------------
physSystParts :: [Sentence]
physSystParts = map (!.)
  [atStartNP (the firstRod) +:+ sParen (S "with" +:+ getTandS lenRod_1),
   atStartNP (the secondRod) +:+ sParen (S "with" +:+ getTandS lenRod_2),
   atStartNP (the firstObject),
   atStartNP (the secondObject)]

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------
-- Assumptions defined in Assumptions

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------
-- Theoretical Models defined in TMs
tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM, newtonSL]

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------
-- General Definitions defined in GDs

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-- Data Definitions defined in DDs

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------
-- Instance Models defined in IMs

-----------------------------
-- 4.2.6 : Data Constraints --
-----------------------------
-- Data Constraints defined in Constraints

-----------------------------
-- 4.2.7 : Properties of a Correct Solution --
-----------------------------
-- Properties of a Correct Solution defined in CorrSolnPpties

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------
-- in Requirements.hs

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

-----------------------------------
-- SECTION 6b : UNLIKELY CHANGES --
-----------------------------------

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

------------------------------------------------
-- Section 8 : Specification Parameter Values --
------------------------------------------------

----------------------------
-- Section 9 : References --
----------------------------
