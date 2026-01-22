module Drasil.PDController.Body (si, mkSRS, pidODEInfo) where

import Language.Drasil
import Drasil.Metadata (dataDefn)
import Drasil.SRSDocument
import Drasil.Generator (cdb)
import qualified Drasil.DocLang.SRS as SRS (inModel)
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (SystemKind(Specification), mkSystem)

import Data.Drasil.Concepts.Math (mathcon', ode)
import Data.Drasil.ExternalLibraries.ODELibraries (odeInfoChunks)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (angular, linear) -- FIXME: should not be needed?
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Math (posInf, negInf)

import Drasil.PDController.Assumptions (assumptions)
import Drasil.PDController.Changes (likelyChgs)
import Drasil.PDController.Concepts (acronyms, pidC, concepts, defs)
import Drasil.PDController.DataDefs (dataDefinitions)
import Drasil.PDController.GenDefs (genDefns)
import Drasil.PDController.LabelledContent (labelledContent, gsdSysContextFig, sysFigure)
import Drasil.PDController.MetaConcepts (progName)
import Drasil.PDController.GenSysDesc
       (gsdSysContextList, gsdSysContextP1, gsdSysContextP2, gsduserCharacteristics)
import Drasil.PDController.IModel (instanceModels, imPD)
import Drasil.PDController.IntroSection (introPara, introPurposeOfDoc, externalLinkRef,
       introUserChar1, introUserChar2, introscopeOfReq, scope)
import Drasil.PDController.References (citations)
import Drasil.PDController.Requirements (funcReqs, nonfuncReqs, funcReqsTables)
import Drasil.PDController.SpSysDesc (goals, sysGoalInput, sysParts)
import Drasil.PDController.TModel (theoreticalModels)
import Drasil.PDController.Unitals (symbols, inputs, outputs, inputsUC,
  inpConstrained, pidConstants)
import Drasil.PDController.ODEs (pidODEInfo)

naveen :: Person
naveen = person "Naveen Ganesh" "Muralidharan"

mkSRS :: SRSDecl
mkSRS
  = [TableOfContents,
    RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA abbreviationsList],
     IntroSec $
       IntroProg introPara (phrase progName)
         [IPurpose [introPurposeOfDoc], IScope introscopeOfReq,
          IChar introUserChar1 introUserChar2 [],
          IOrgSec dataDefn (SRS.inModel [] [])
            (S "The instance model referred as" +:+ refS imPD +:+
               S "provides an"
               +:+ titleize ode +:+ sParen (short ode)
               +:+ S "that models the"
               +:+ phrase pidC)],
     GSDSec $
       GSDProg
         [SysCntxt
            [gsdSysContextP1, LlC gsdSysContextFig, gsdSysContextP2,
             gsdSysContextList],
          UsrChars [gsduserCharacteristics], SystCons [] []],
     SSDSec $
       SSDProg
         [SSDProblem $
            PDProg purp []
              [TermsAndDefs Nothing defs,
               PhySysDesc progName sysParts sysFigure [],
               Goals sysGoalInput],
          SSDSolChSpec $
            SCSProg
              [Assumptions, TMs [] (Label : stdFields),
               GDs [] (Label : stdFields) HideDerivation,
               DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation,
               IMs []
                 ([Label, Input, Output, InConstraints, OutConstraints] ++
                    stdFields)
                 ShowDerivation,
               Constraints EmptyS inputsUC]],

     ReqrmntSec $ ReqsProg [FReqsSub funcReqsTables, NonFReqsSub], LCsSec,
     TraceabilitySec $ TraceabilityProg $ traceMatStandard si, Bibliography]

si :: System
si = mkSystem
  progName Specification [naveen]
  [purp] [background] [scope] [motivation]
  theoreticalModels genDefns dataDefinitions instanceModels
  []
  inputs outputs (map cnstrw' inpConstrained)
  pidConstants symbMap allRefs

purp :: Sentence
purp = foldlSent_ [S "provide a model" `S.ofA` phrase pidC,
         S "that can be used for the tuning" `S.ofThe` S "gain constants before",
         S "the deployment" `S.ofThe` S "controller"]

motivation :: Sentence
motivation = foldlSent_ [S "The gains of a controller in an application" +:+
              S "must be tuned before the controller is ready for production"]

background :: Sentence
background = foldlSent_ [S "Automatic process control with a controller (P/PI/PD/PID) is used",
              S "in a variety of applications such as thermostats, automobile",
              S "cruise-control, etc"]

-- FIXME: 'symbolsWCodeSymbols' shouldn't exist. See DblPend's discussion of its
-- 'symbolsWCodeSymbols'.
symbolsWCodeSymbols :: [DefinedQuantityDict]
symbolsWCodeSymbols = symbols ++ map dqdWr pidConstants
  ++ odeInfoChunks pidODEInfo

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  concepts ++
  -- CIs
  nw progName : map nw mathcon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  physicalcon ++ [linear, angular]

symbMap :: ChunkDB
symbMap = cdb (map dqdWr physicscon ++ symbolsWCodeSymbols ++ [dqdWr mass, dqdWr posInf, dqdWr negInf])
  ideaDicts
  conceptChunks
  ([] :: [UnitDefn])
  dataDefinitions
  instanceModels
  genDefns
  theoreticalModels
  conceptInstances
  citations
  (labelledContent ++ funcReqsTables)

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef]

abbreviationsList  :: [IdeaDict]
abbreviationsList  =
  -- CIs
  map nw acronyms ++
  -- QuantityDicts
  map nw symbols

conceptInstances :: [ConceptInstance]
conceptInstances = assumptions ++ goals ++ funcReqs ++ nonfuncReqs ++ likelyChgs

stdFields :: Fields
stdFields
  = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
