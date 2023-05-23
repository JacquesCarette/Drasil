{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.Body where

import Language.Drasil hiding (organization, section)
import Theory.Drasil (TheoryModel)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS

import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.People (dong)
import Data.Drasil.SI_Units (metre, second, newton, kilogram, degree, radian, hertz)
import Data.Drasil.Concepts.Computation (inDatum, compcon, inValue, algorithm)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs, physics, variable)
import Data.Drasil.Concepts.Documentation (assumption, condition, endUser, environment, datum, document,
  input_, interface, output_, organization, problem, product_, physical, sysCont, software, softwareConstraint,
  softwareSys, srsDomains, system, template, user, doccon, doccon', analysis)
import Data.Drasil.Concepts.Education (highSchoolPhysics, highSchoolCalculus, calculus, undergraduate, educon, )
import Data.Drasil.Concepts.Math (mathcon, cartesian, ode, mathcon', graph)
import Data.Drasil.Concepts.Physics (gravity, physicCon, physicCon', pendulum, twoD, motion)
import Data.Drasil.Concepts.PhysicalProperties (mass, len, physicalcon)
import Data.Drasil.Concepts.Software (program, errMsg)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Quantities.Math (unitVect, unitVectj)
import Data.Drasil.Software.Products (prodtcon, sciCompS)
import Data.Drasil.Theories.Physics (newtonSL, accelerationTM, velocityTM, newtonSLR)
import Data.Drasil.TheoryConcepts (inModel)

import Drasil.DblPendulum.Figures (figMotion, sysCtxFig1)
import Drasil.DblPendulum.Assumptions (assumpDouble)
import Drasil.DblPendulum.Concepts (rod, concepts, pendMotion, progName, firstRod, secondRod, firstObject, secondObject)
import Drasil.DblPendulum.Goals (goals, goalsInputs)
import Drasil.DblPendulum.DataDefs (dataDefs)
import Drasil.DblPendulum.IMods (iMods)
import Drasil.DblPendulum.GenDefs (genDefns)
import Drasil.DblPendulum.Unitals (lenRod_1, lenRod_2, symbols, inputs, outputs,
  inConstraints, outConstraints, acronyms, pendDisAngle, constants)
import Drasil.DblPendulum.Requirements (funcReqs, nonFuncReqs)
import Drasil.DblPendulum.References (citations, koothoor2013, smithEtAl2007,
 smithLai2005, smithKoothoor2016)
import Data.Drasil.ExternalLibraries.ODELibraries (scipyODESymbols, osloSymbols, apacheODESymbols, odeintSymbols, arrayVecDepVar)
import Language.Drasil.Code (quantvar)
import Drasil.DblPendulum.ODEs (dblPenODEInfo)


srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [TableOfContents, -- This creates the Table of Contents
  RefSec $      --This creates the Reference section of the SRS
    RefProg intro      -- This add the introduction blob to the reference section  
      [ TUnits         -- Adds table of unit section with a table frame
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits] -- Adds table of symbol section with a table frame
      --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
      , TAandA         -- Add table of abbreviation and acronym section
      ],
  IntroSec $
    IntroProg justification (phrase progName)
      [IPurpose $ purpDoc progName Verbose,
       IScope scope,
       IChar [] charsOfReader [],
       IOrgSec organizationOfDocumentsIntro inModel (SRS.inModel [] []) EmptyS],
  GSDSec $ 
    GSDProg [
      SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
      UsrChars [userCharacteristicsIntro], 
      SystCons [] []],                            
  SSDSec $ 
    SSDProg
      [ SSDProblem $ PDProg purp []                --  This adds a is used to define the problem your system will solve
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
     AuxConsProg progName [],  --Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

si :: SystemInformation
si = SI {
  _sys         = progName, 
  _kind        = Doc.srs,
  _authors     = [dong],
  _purpose     = [purp],
  _background  = [], 
  _quants      = symbolsAll,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = inConstraints,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

purp :: Sentence
purp = foldlSent_ [S "efficiently and correctly predict the", phraseNP (motion `ofA` pendulum)]

symbolsAll :: [QuantityDict]
symbolsAll = symbols ++ scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols 
  ++ map qw [listToArray $ quantvar pendDisAngle, arrayVecDepVar dblPenODEInfo]

symbMap :: ChunkDB
symbMap = cdb (map qw iMods ++ map qw symbolsAll)
  (nw newtonSLR : nw progName : nw mass : nw len : nw kilogram : nw inValue : nw newton : nw degree : nw radian
    : nw unitVect : nw unitVectj : [nw errMsg, nw program] ++ map nw symbols ++
   map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw physicCon' ++
   map nw physicscon ++ concepts ++ map nw physicalcon ++ map nw acronyms ++ map nw symbols ++ map nw [metre, hertz] ++
   [nw algorithm] ++ map nw compcon ++ map nw educon ++ map nw prodtcon)
  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz])
  dataDefs iMods genDefns tMods concIns [] [] ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbols) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumpDouble ++ goals ++ funcReqs ++ nonFuncReqs
-- ++ likelyChgs ++ unlikelyChgs

------------------------------
-- Section : INTRODUCTION --
------------------------------
justification :: Sentence
justification = foldlSent [ atStartNP (a_ pendulum), S "consists" `S.of_` phrase mass, 
                            S "attached to the end" `S.ofA` phrase rod `S.andIts` S "moving curve" `S.is`
                            (S "highly sensitive to initial conditions" !.), S "Therefore" `sC`
                            S "it is useful to have a", phrase program, S "to simulate", phraseNP (motion
                            `the_ofThe` pendulum), (S "to exhibit its chaotic characteristics" !.),
                            atStartNP (the program), S "documented here is called", phrase progName]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope :: Sentence
scope = foldlSent_ [phraseNP (NP.the (analysis `ofA` twoD)), 
  sParen (getAcc twoD), phrase pendMotion, phrase problem,
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
organizationOfDocumentsIntro :: Sentence
organizationOfDocumentsIntro = foldlSent 
  [atStartNP (the organization), S "of this", phrase document, 
  S "follows the", phrase template, S "for an", getAcc Doc.srs, S "for", 
  phrase sciCompS, S "proposed by", foldlList Comma List $ 
    map refS [koothoor2013, smithLai2005, smithEtAl2007 , smithKoothoor2016]]


--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
-- Description of Genreal System automatically generated in GSDProg

--------------------------
-- 3.1 : System Context --
--------------------------
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1, S "shows the" +:+. phrase sysCont,
   S "A circle represents an entity external to the", phrase software
   `sC` phraseNP (the user), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", sParen (short progName) +:+. EmptyS,
   S "Arrows are used to show the data flow between the", phraseNP (system
   `andIts` environment)]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol [S "The interaction between the", phraseNP (product_
   `andThe` user), S "is through an application programming" +:+.
   phrase interface, S "The responsibilities of the", phraseNP (user 
   `andThe` system), S "are as follows"]

sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide initial" +:+ pluralNP (condition `ofThePS`
  physical) +:+ S "state of the" +:+ phrase motion +:+ S "and the" +:+ plural inDatum +:+ S "related to the" +:+
  phrase progName `sC` S "ensuring no errors in the" +:+
  plural datum +:+. S "entry",
  S "Ensure that consistent units are used for" +:+. pluralNP (combineNINI input_ Doc.variable),
  S "Ensure required" +:+
  namedRef (SRS.assumpt ([]::[Contents]) ([]::[Section])) (phrase software +:+ plural assumption) +:+
  S "are appropriate for any particular" +:+
  phrase problem +:+ S "input to the" +:+. phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters" +:+
  phrase input_ +:+. S "instead of a floating point number",
  S "Determine if the" +:+ plural input_ +:+ S "satisfy the required" +:+.
  pluralNP (physical `and_` softwareConstraint),
  S "Calculate the required" +:+. plural output_, 
  S "Generate the required" +:+. plural graph]

sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short progName +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------
userCharacteristicsIntro :: Contents
userCharacteristicsIntro = foldlSP
  [S "The", phrase endUser `S.of_` short progName,
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

--------------------------------
-- SECTION 6b : UNLIKELY CHANGES --
--------------------------------

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------

----------------------------
-- Section 9 : References --
----------------------------
