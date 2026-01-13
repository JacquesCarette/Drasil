{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.Body (mkSRS, si) where

import Control.Lens ((^.))

import Drasil.Metadata (inModel)
import Language.Drasil hiding (organization, section)
import qualified Language.Drasil.Development as D
import Theory.Drasil (TheoryModel, output)
import Drasil.SRSDocument
import Drasil.Generator (cdb)
import qualified Drasil.DocLang.SRS as SRS
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (SystemKind(Specification), mkSystem)

import Data.Drasil.People (olu)
import Data.Drasil.Concepts.Math (mathcon')
import Data.Drasil.Concepts.Physics (physicCon', motion, pendulum, angular, displacement, iPos, gravitationalConst, gravity, rigidBody, weight, shm)
import Data.Drasil.Concepts.PhysicalProperties (mass, physicalcon)
import Data.Drasil.Quantities.PhysicalProperties (len)
import Data.Drasil.Theories.Physics (newtonSLR)

import Drasil.DblPend.Body (justification, externalLinkRef, charsOfReader,
  sysCtxIntro, sysCtxDesc, sysCtxList, stdFields, scope, terms,
  userCharacteristicsIntro)
import qualified Drasil.DblPend.Body as DPD (tMods)
import Drasil.DblPend.Concepts (concepts, rod)
import Drasil.DblPend.Requirements (nonFuncReqs)
import Drasil.DblPend.Unitals (acronyms)
import Drasil.DblPend.References (citations)

import Drasil.SglPend.Assumptions (assumpSingle)
import Drasil.SglPend.Goals (goals, goalsInputs)
import Drasil.SglPend.DataDefs (dataDefs)
import Drasil.SglPend.IMods (iMods)
import Drasil.SglPend.LabelledContent (figMotion, sysCtxFig1, labelledContent)
import Drasil.SglPend.MetaConcepts (progName)
import Drasil.SglPend.GenDefs (genDefns)
import Drasil.SglPend.Unitals (inputs, outputs, inConstraints, outConstraints, symbols)
import Drasil.SglPend.Requirements (funcReqs, funcReqsTables)

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
        , Constraints EmptyS (map (`uq` defaultUncrt) inConstraints) -- FIXME: Why do all values in this table need a reasonable value?
        , CorrSolnPpties outConstraints []
       ]
     ],
  ReqrmntSec $ ReqsProg
    [ FReqsSub funcReqsTables
    , NonFReqsSub
    ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg progName [],  -- Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

si :: System
si = mkSystem progName Specification [olu]
  [purp] [] [] []
  tMods genDefns dataDefs iMods
  []
  inputs outputs inConstraints []
  symbMap allRefs

purp :: Sentence
purp = foldlSent_ [S "predict the", phrase motion `S.ofA` S "single", phrase pendulum]

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  concepts ++
  -- CIs
  nw progName : map nw mathcon' ++ map nw physicCon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  physicalcon ++ [angular, displacement, iPos, pendulum, motion,
  gravitationalConst, gravity, rigidBody, weight, shm] ++
  -- Unital Chunks
  [cw len]

abbreviationsList :: [IdeaDict]
abbreviationsList =
  -- CIs
  nw progName : map nw acronyms ++
  -- QuantityDicts
  map nw symbols

symbMap :: ChunkDB
symbMap = cdb (map (^. output) iMods ++ symbols) ideaDicts conceptChunks []
  dataDefs iMods genDefns tMods concIns citations
  (labelledContent ++ funcReqsTables)

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
physSystParts = map ((!.) . D.toSent . atStartNP) [the rod, the mass]

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
