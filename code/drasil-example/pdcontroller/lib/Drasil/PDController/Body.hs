module Drasil.PDController.Body (pidODEInfo, printSetting, si, srs, fullSI) where

import Language.Drasil
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS (inModel)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Utils.Drasil.Sentence as S

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

import Drasil.PDController.Assumptions (assumptions)
import Drasil.PDController.Changes (likelyChgs)
import Drasil.PDController.Concepts (acronyms, pidControllerSystem,
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
  inpConstrained, pidConstants, pidDqdConstants)
import Drasil.PDController.ODEs (pidODEInfo)

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
  = [
    TableOfContents,
    RefSec $ RefProg intro,
      TUnits TUProg, 
      TSymb $ tsymb [TSPurpose, SymbOrder], 
      TAandA TAAProg,
    IntroSec $ IntroProg introPara (phrase pidControllerSystem),
      IPurposeSub $ IPurposeProg [introPurposeOfDoc], 
      IScopeSub $ IScopeProg introscopeOfReq,
      ICharSub $ ICharProg introUserChar1 introUserChar2 [],
      IOrgSub $ IOrgProg introDocOrg IDict.dataDefn (SRS.inModel 0 [])
        (S "The instance model referred as" +:+ refS imPD +:+
         S "provides an" +:+ titleize ode +:+ sParen (short ode) +:+ S "that models the" +:+ phrase pidC),
    GSDSec $ GSDProg EmptyS,
      SysCntxt $ SysCntxtProg [gsdSysContextP1, LlC gsdSysContextFig, gsdSysContextP2, gsdSysContextList],
      UsrChars $ UsrCharsProg [gsduserCharacteristics], 
      SystCons $ SystConsProg [],
    SSDSec $ SSDProg EmptyS,
      ProblemDescription $ PDProg sysProblemDesc,
        TermsAndDefs $ TDProg Nothing defs,
        PhySysDesc $ PSDProg pidControllerSystem sysParts sysFigure [],
        Goals $ GProg sysGoalInput,
      SolChSpec $ SCSProg EmptyS,
        Assumptions $ AssumpProg EmptyS, 
        TMs $ TMProg [] (Label : stdFields),
        GDs $ GDProg [] (Label : stdFields) HideDerivation,
        DDs $ DDProg [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation,
        IMs $ IMProg [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation,
        Constraints $ ConstProg EmptyS inputsUC,
    ReqrmntSec $ ReqsProg EmptyS,
      FReqsSub $ FReqsProg EmptyS [], 
      NonFReqsSub NonFReqsProg, 
    LCsSec,
    TraceabilitySec $ TraceabilityProg $ traceMatStandard si, Bibliography]

si :: SystemInformation
si
  = SI{_sys = pidControllerSystem, _kind = Doc.srs, _authors = [naveen],
       _purpose = [], _quants = symbolsAll,
       _concepts = [] :: [DefinedQuantityDict],
       _datadefs = dataDefinitions, _instModels = [],
       _configFiles = [], _inputs = inputs, _outputs = outputs,
       _defSequence = [] :: [Block SimpleQDef],
       _constraints = map cnstrw inpConstrained, _constants = pidConstants,
       _sysinfodb = symbMap, _usedinfodb = usedDB, refdb = refDB}

symbolsAll :: [QuantityDict]
symbolsAll
  = symbols ++
      scipyODESymbols ++
        osloSymbols ++
          apacheODESymbols ++
            odeintSymbols ++
              map qw [arrayVecDepVar pidODEInfo] ++
                map qw pidDqdConstants ++ map qw pidConstants

symbMap :: ChunkDB
symbMap
  = cdb (map qw physicscon ++ symbolsAll ++ map qw [mass])
      (nw pidControllerSystem :
         [nw program, nw angular, nw linear] ++
           map nw doccon ++
             map nw doccon' ++
               concepts ++
                 map nw mathcon ++
                   map nw mathcon' ++
                     map nw [second, kilogram] ++
                       map nw symbols ++ map nw physicscon ++ map nw acronyms
                       ++ map nw physicalcon)
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
usedDB
  = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbolsAll)
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
