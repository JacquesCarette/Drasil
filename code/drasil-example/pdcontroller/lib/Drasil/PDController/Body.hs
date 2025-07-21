module Drasil.PDController.Body (pidODEInfo, printSetting, si, srs, fullSI) where

import Language.Drasil
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS (inModel)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Math (mathcon, mathcon', ode)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.ExternalLibraries.ODELibraries
       (apacheODESymbols, arrayVecDepVar, odeintSymbols, osloSymbols,
        scipyODESymbols)
import qualified Data.Drasil.TheoryConcepts as IDict (dataDefn)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (angular, linear) -- FIXME: should not be needed?
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.SI_Units (siUnits)
import Data.Drasil.Quantities.Math (posInf, negInf)

import Drasil.PDController.Assumptions (assumptions)
import Drasil.PDController.Changes (likelyChgs)
import Drasil.PDController.Concepts (acronyms, pidC, concepts, defs)
import Drasil.PDController.DataDefs (dataDefinitions)
import Drasil.PDController.GenDefs (genDefns)
import Drasil.PDController.MetaConcepts (progName)
import Drasil.PDController.GenSysDesc
       (gsdSysContextFig, gsdSysContextList, gsdSysContextP1, gsdSysContextP2,
        gsduserCharacteristics)
import Drasil.PDController.IModel (instanceModels, imPD)
import Drasil.PDController.IntroSection (introPara, introPurposeOfDoc, externalLinkRef,
       introUserChar1, introUserChar2, introscopeOfReq, scope)
import Drasil.PDController.References (citations)
import Drasil.PDController.Requirements (funcReqs, nonfuncReqs)
import Drasil.PDController.SpSysDesc (goals, sysFigure, sysGoalInput, sysParts)
import Drasil.PDController.TModel (theoreticalModels)
import Drasil.PDController.Unitals (symbols, inputs, outputs, inputsUC,
  inpConstrained, pidConstants, opProcessVariable)
import Drasil.PDController.ODEs (pidODEInfo)
import Language.Drasil.Code (quantvar)

naveen :: Person
naveen = person "Naveen Ganesh" "Muralidharan"

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS
  = [TableOfContents,
    RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA abbreviationsList],
     IntroSec $
       IntroProg introPara (phrase progName)
         [IPurpose [introPurposeOfDoc], IScope introscopeOfReq,
          IChar introUserChar1 introUserChar2 [],
          IOrgSec IDict.dataDefn (SRS.inModel [] [])
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
si = SI {
  _sys = progName,
  _kind = Doc.srs,
  _authors = [naveen],
  _purpose = [purp],
  _background  = [background],
  _motivation  = [motivation],
  _scope       = [scope],
  _quants = symbolsAll,
  _datadefs = dataDefinitions,
  _instModels = instanceModels,
  _configFiles = [],
  _inputs = inputs,
  _outputs = outputs,
  _constraints = map cnstrw inpConstrained,
  _constants = pidConstants,
  _systemdb = symbMap
}

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

symbolsAll :: [DefinedQuantityDict]
symbolsAll = symbols ++ map dqdWr pidConstants
  ++ scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols 
  ++ map dqdWr [listToArray $ quantvar opProcessVariable, arrayVecDepVar pidODEInfo]

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  sciCompS : concepts ++ doccon ++
  -- CIs
  nw progName : map nw mathcon' ++ map nw doccon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  physicalcon ++ mathcon ++ [linear, program, angular] ++ srsDomains ++
  -- ConstrConcepts
  map cw inpConstrained

symbMap :: ChunkDB
symbMap = cdb (map dqdWr physicscon ++ symbolsAll ++ [dqdWr mass, dqdWr posInf, dqdWr negInf])
  ideaDicts
  conceptChunks
  siUnits
  dataDefinitions
  instanceModels
  genDefns
  theoreticalModels
  conceptInstances
  ([] :: [LabelledContent])
  allRefs
  citations

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef]

abbreviationsList  :: [IdeaDict]
abbreviationsList  =
  -- CIs
  map nw acronyms ++
  -- QuantityDicts
  map nw symbolsAll
  
conceptInstances :: [ConceptInstance]
conceptInstances = assumptions ++ goals ++ funcReqs ++ nonfuncReqs ++ likelyChgs

stdFields :: Fields
stdFields
  = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
