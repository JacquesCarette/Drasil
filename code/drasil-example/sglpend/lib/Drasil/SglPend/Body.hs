{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.Body where

import Control.Lens ((^.))

import Language.Drasil hiding (organization, section)
import Theory.Drasil (TheoryModel, output)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.People (olu)
import Data.Drasil.SI_Units (siUnits)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Documentation (srsDomains, doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.Physics (physicCon, physicCon', motion, pendulum)
import Data.Drasil.Concepts.PhysicalProperties (mass, physicalcon)
import Data.Drasil.Concepts.Software (program, errMsg)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Theories.Physics (newtonSLR)
import Data.Drasil.TheoryConcepts (inModel)

import Drasil.DblPend.Body (justification, externalLinkRef, charsOfReader,
  sysCtxIntro, sysCtxDesc, sysCtxList, stdFields, scope, terms,
  userCharacteristicsIntro)
import qualified Drasil.DblPend.Body as DPD (tMods)
import Drasil.DblPend.Concepts (concepts, rod)
import Drasil.DblPend.Requirements (nonFuncReqs)
import Drasil.DblPend.Unitals (acronyms)
import Drasil.DblPend.References (citations)

import Drasil.SglPend.Assumptions (assumpSingle)
import Drasil.SglPend.Figures (figMotion, sysCtxFig1)
import Drasil.SglPend.Goals (goals, goalsInputs)
import Drasil.SglPend.DataDefs (dataDefs)
import Drasil.SglPend.IMods (iMods)
import Drasil.SglPend.MetaConcepts (progName)
import Drasil.SglPend.GenDefs (genDefns)
import Drasil.SglPend.Unitals (inputs, outputs, inConstraints, outConstraints, symbols)
import Drasil.SglPend.Requirements (funcReqs)

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
      --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
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
     AuxConsProg progName [],  -- Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

si :: System
si = SI {
  _sys         = progName, 
  _kind        = Doc.srs,
  _authors     = [olu],
  _purpose     = [purp],
  _background  = [],
  _scope       = [],
  _motivation  = [],
  _quants      = symbols,
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _constraints = inConstraints,
  _constants   = [] :: [ConstQDef],
  _systemdb   = symbMap
}

purp :: Sentence
purp = foldlSent_ [S "predict the", phrase motion `S.ofA` S "single", phrase pendulum]

ideaDicts :: [IdeaDict]
ideaDicts = 
  -- Actual IdeaDicts
  doccon ++ concepts ++ compcon ++ educon ++ prodtcon ++
  -- CIs
  nw progName : map nw doccon' ++ map nw mathcon' ++ map nw physicCon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  [errMsg, program, algorithm] ++ physicCon ++ physicalcon ++ mathcon ++ srsDomains

abbreviationsList :: [IdeaDict]
abbreviationsList =
  -- CIs
  nw progName : map nw acronyms ++
  -- QuantityDicts
  map nw symbols

symbMap :: ChunkDB
symbMap = cdb (map (^. output) iMods ++ map qw symbols) ideaDicts conceptChunks
  siUnits dataDefs iMods genDefns tMods concIns [] allRefs citations

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef]

concIns :: [ConceptInstance]
concIns = assumpSingle ++ goals ++ funcReqs ++ nonFuncReqs

------------------------------
-- Section : INTRODUCTION --
------------------------------

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.4 : Organization of Documents --
-------------------------------------

-------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
-------------------------------------------
-- Description of Genreal System automatically generated in GSDProg

--------------------------
-- 3.1 : System Context --
--------------------------

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

------------------------------
-- 3.3 : System Constraints --
------------------------------
-- System Constraints automatically generated in SystCons


--------------------------------------------
-- Section 4: Specific System Description --
--------------------------------------------
-- Description of Specific System automatically generated in SSDProg

------------------------------
-- 4.1 : System Constraints --
------------------------------

---------------------------------
-- 4.1.1 Terminology and Definitions --
---------------------------------

-----------------------------------
-- 4.1.2 Physical System Description --
-----------------------------------
physSystParts :: [Sentence]
physSystParts = map ((!.) . atStartNP) [the rod, the mass]

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
tMods = DPD.tMods ++ [newtonSLR]

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

------------------------------------------------
-- Section 8 : Specification Parameter Values --
------------------------------------------------

----------------------------
-- Section 9 : References --
----------------------------
