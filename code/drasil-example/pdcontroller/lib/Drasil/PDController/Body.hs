module Drasil.PDController.Body (pidODEInfo, printSetting, si, srs, fullSI) where

import Language.Drasil
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS (inModel)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Math (mathcon, mathcon', ode)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.ExternalLibraries.ODELibraries
       (apacheODESymbols, arrayVecDepVar, odeintSymbols, osloSymbols,
        scipyODESymbols)
import qualified Data.Drasil.TheoryConcepts as IDict (dataDefn)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (angular, linear) -- FIXME: should not be needed?
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.SI_Units (second, kilogram)
import Data.Drasil.Quantities.Math (posInf, negInf)

import Drasil.PDController.Assumptions (assumptions)
import Drasil.PDController.Changes (likelyChgs)
import Drasil.PDController.Concepts (acronyms, pdControllerApp,
  pidC, concepts, defs)
import Drasil.PDController.DataDefs (dataDefinitions)
import Drasil.PDController.GenDefs (genDefns)
import Drasil.PDController.GenSysDesc
       (gsdSysContextFig, gsdSysContextList, gsdSysContextP1, gsdSysContextP2,
        gsduserCharacteristics)
import Drasil.PDController.IModel (instanceModels, imPD)
import Drasil.PDController.IntroSection
       (introDocOrg, introPara, introPurposeOfDoc, introUserChar1,
        introUserChar2, introscopeOfReq)
import Drasil.PDController.References (citations)
import Drasil.PDController.Requirements (funcReqs, nonfuncReqs)
import Drasil.PDController.SpSysDesc
       (goals, sysFigure, sysGoalInput, sysParts, sysProblemDesc)
import Drasil.PDController.TModel (theoreticalModels)
import Drasil.PDController.Unitals (symbols, inputs, outputs, inputsUC,
  inpConstrained, pidConstants, pidDqdConstants, opProcessVariable)
import Drasil.PDController.ODEs (pidODEInfo)
import Language.Drasil.Code (quantvar)

naveen :: Person
naveen = person "Naveen Ganesh" "Muralidharan"

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS
  = [TableOfContents,
    RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA],
     IntroSec $
       IntroProg introPara (phrase pdControllerApp)
         [IPurpose [introPurposeOfDoc], IScope introscopeOfReq,
          IChar introUserChar1 introUserChar2 [],
          IOrgSec introDocOrg IDict.dataDefn (SRS.inModel [] [])
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
            PDProg sysProblemDesc []
              [TermsAndDefs Nothing defs,
               PhySysDesc pdControllerApp sysParts sysFigure [],
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

si :: SystemInformation
si = SI {
  _sys = pdControllerApp,
  _kind = Doc.srs,
  _authors = [naveen],
  _purpose = [],
  _background  = [],
  _quants = symbolsAll,
  _concepts = [] :: [DefinedQuantityDict],
  _datadefs = dataDefinitions,
  _instModels = instanceModels,
  _configFiles = [],
  _inputs = inputs,
  _outputs = outputs,
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = map cnstrw inpConstrained,
  _constants = pidConstants,
  _sysinfodb = symbMap,
  _usedinfodb = usedDB,
   refdb = refDB}

symbolsAll :: [QuantityDict]
symbolsAll = symbols ++ map qw pidDqdConstants ++ map qw pidConstants
  ++ scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols 
  ++ map qw [listToArray $ quantvar opProcessVariable, arrayVecDepVar pidODEInfo]

symbMap :: ChunkDB
symbMap = cdb (map qw physicscon ++ symbolsAll ++ [qw mass, qw posInf, qw negInf])
  (nw pdControllerApp : [nw program, nw angular, nw linear]
  ++ map nw doccon ++ map nw doccon' ++ concepts ++ map nw mathcon
  ++ map nw mathcon' ++ map nw [second, kilogram] ++ map nw symbols 
  ++ map nw physicscon ++ map nw acronyms ++ map nw physicalcon)
  (map cw inpConstrained ++ srsDomains)
  (map unitWrapper [second, kilogram])
  dataDefinitions
  instanceModels
  genDefns
  theoreticalModels
  conceptInstances
  ([] :: [Section])
  ([] :: [LabelledContent])
  ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbolsAll)
  ([] :: [ConceptChunk])
  ([] :: [UnitDefn])
  ([] :: [DataDefinition])
  ([] :: [InstanceModel])
  ([] :: [GenDefn])
  ([] :: [TheoryModel])
  ([] :: [ConceptInstance])
  ([] :: [Section])
  ([] :: [LabelledContent])
  ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations conceptInstances

conceptInstances :: [ConceptInstance]
conceptInstances = assumptions ++ goals ++ funcReqs ++ nonfuncReqs ++ likelyChgs

stdFields :: Fields
stdFields
  = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
