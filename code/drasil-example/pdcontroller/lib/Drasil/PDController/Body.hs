module Drasil.PDController.Body (pidODEInfo, printSetting, si, srs, fullSI) where

import Control.Lens ((^.))

import Language.Drasil
import Drasil.Metadata (dataDefn)
import Drasil.SRSDocument
import Drasil.Generator (cdb)
import qualified Drasil.DocLang.SRS as SRS (inModel)
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (SystemKind(Specification), mkSystem, systemdb)

import Data.Drasil.Concepts.Math (mathcon', ode)
import Data.Drasil.ExternalLibraries.ODELibraries
       (apacheODESymbols, odeintSymbols, osloSymbols,
        scipyODESymbols, odeInfoChunks)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (angular, linear) -- FIXME: should not be needed?
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Math (posInf, negInf)

import Drasil.PDController.Assumptions (assumptions)
import Drasil.PDController.Changes (likelyChgs)
import Drasil.PDController.Concepts (pidC, concepts, defs, pidCI, proportionalCI, acronyms, pdControllerCI)
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
import Drasil.PDController.Requirements (funcReqs, nonfuncReqs)
import Drasil.PDController.SpSysDesc (goals, sysGoalInput, sysParts)
import Drasil.PDController.TModel (theoreticalModels)
import Drasil.PDController.Unitals (symbols, inputs, outputs, inputsUC,
  inpConstrained, pidConstants)
import Drasil.PDController.ODEs (pidODEInfo)

naveen :: Person
naveen = person "Naveen Ganesh" "Muralidharan"

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) fullSI

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys (fullSI ^. systemdb) Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS
  = [TableOfContents,
    RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA ],
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

     ReqrmntSec $ ReqsProg [FReqsSub EmptyS [], NonFReqsSub], LCsSec,
     TraceabilitySec $ TraceabilityProg $ traceMatStandard si, Bibliography]

si :: System
si = mkSystem
  progName Specification [naveen]
  [purp] [background] [scope] [motivation]
  symbolsAll
  theoreticalModels genDefns dataDefinitions instanceModels
  []
  inputs outputs (map cnstrw' inpConstrained)
  pidConstants symbMap

purp :: Sentence
purp = foldlSent_ [S "provide a model" `S.ofA` phrase pidC,
         S "that can be used for the tuning" `S.ofThe` S "gain constants before",
         S "the deployment" `S.ofThe` S "controller"]

motivation :: Sentence
motivation = foldlSent_ [S "The gains of a controller in an application" +:+
              S "must be tuned before the controller is ready for production"]

background :: Sentence
background = foldlSent_ [
  S "Automatic process control with a controller ("
    :+: short proportionalCI :+: S "/PI/" :+: short pdControllerCI :+: S "/" :+: short pidCI :+: S ") is used",
  S "in a variety of applications such as thermostats, automobile",
  S "cruise-control, etc"]

-- FIXME: the dependent variable of pidODEInfo (opProcessVariable) is currently added to symbolsAll automatically as it is used to create new chunks with opProcessVariable's UID suffixed in ODELibraries.hs.
-- The correct way to fix this is to add the chunks when they are created in the original functions. See #4298 and #4301
symbolsAll :: [DefinedQuantityDict]
symbolsAll = symbols ++ map dqdWr pidConstants
  ++ scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols
  ++ odeInfoChunks pidODEInfo

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  concepts ++
  -- CIs
  nw progName : map nw mathcon' ++ map nw acronyms

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  physicalcon ++ [linear, angular]

symbMap :: ChunkDB
symbMap = cdb (map dqdWr physicscon ++ symbolsAll ++ [dqdWr mass, dqdWr posInf, dqdWr negInf])
  ideaDicts
  conceptChunks
  ([] :: [UnitDefn])
  dataDefinitions
  instanceModels
  genDefns
  theoreticalModels
  conceptInstances
  labelledContent
  allRefs
  citations

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef]

conceptInstances :: [ConceptInstance]
conceptInstances = assumptions ++ goals ++ funcReqs ++ nonfuncReqs ++ likelyChgs

stdFields :: Fields
stdFields
  = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
