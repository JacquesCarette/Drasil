module Drasil.DocLang.SRS
 (doc, doc', prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient,
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, thModel,
  genDefn, inModel, dataDefn, datCon, require, nonfuncReq, funcReq, likeChg, unlikeChg, 
  appendix, propCorSol, offShelfSol, valsOfAuxCons, traceyMandG,
  physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, valsOfAuxConsLabel,
  indPRCaseLabel, unlikeChgLabel, funcReqLabel,
  solCharSpecLabel) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil
import qualified Data.Drasil.Concepts.Documentation as Doc (appendix, 
    assumption, charOfIR, client, customer, consVals, dataDefn, datumConstraint, 
    functionalRequirement, genDefn, generalSystemDescription, goalStmt, 
    indPRCase, inModel, likelyChg, unlikelyChg, nonfunctionalRequirement,
    offShelfSolution, orgOfDoc, physSyst, prodUCTable, problemDescription, 
    propOfCorSol, prpsOfDoc, requirement, scpOfReq, scpOfTheProj,
    solutionCharSpec, specificsystemdescription, srs, stakeholder, sysCont, 
    systemConstraint, termAndDef, terminology, thModel, traceyMandG,
    userCharacteristic)
import Data.Drasil.Phrase (for'', the, the')

import Drasil.DocLang.GenBuilders (section')

import Control.Lens ((^.))

-- Local function to keep things looking clean, not exported.
forTT :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT = for'' titleize' titleize

forTT' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT' = for'' titleize' titleize'

-- | SRS document constructor. 
-- Create the SRS from given system name, authors, and sections
doc, doc' :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc  sys = Document (Doc.srs `forTT` sys)
-- | Uses plural of system for title.
doc' sys = Document (Doc.srs `forTT'` sys)

-- | Standard SRS section builders
prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, thModel, traceyMandG,
  genDefn, inModel, dataDefn, datCon, propCorSol, require, nonfuncReq, funcReq, likeChg,
  appendix, offShelfSol, valsOfAuxCons, unlikeChg :: [Contents] -> [Section] -> Section

prpsOfDoc    cs ss = section' (titleize Doc.prpsOfDoc) cs ss "DocPurpose"
scpOfReq     cs ss = section' (titleize Doc.scpOfReq)  cs ss "ReqsScope"
charOfIR     cs ss = section' (titleize' Doc.charOfIR) cs ss "ReaderChars"
orgOfDoc     cs ss = section' (titleize Doc.orgOfDoc)  cs ss "DocOrg"

stakeholder  cs ss = section' (titleize' Doc.stakeholder) cs ss "Stakeholder"
theCustomer  cs ss = section' (titleize' Doc.customer)    cs ss "Customer"
theClient    cs ss = section' (titleize' Doc.client)      cs ss "Client"

genSysDes    cs ss = section' (titleize Doc.generalSystemDescription) cs ss "GenSysDesc"
sysCont      cs ss = section' (titleize Doc.sysCont)              cs ss  "SysContext"
userChar     cs ss = section' (titleize' Doc.userCharacteristic)  cs ss  "UserChars"
sysCon       cs ss = section' (titleize' Doc.systemConstraint)    cs ss  "SysConstraints"

scpOfTheProj cs ss = section' (at_start (Doc.scpOfTheProj titleize)) cs ss "ProjScope"
prodUCTable  cs ss = section' (titleize Doc.prodUCTable)      cs ss      "UseCaseTable"
indPRCase    cs ss = section (titleize' Doc.indPRCase)       cs ss      indPRCaseLabel

specSysDes   cs ss = section' (titleize Doc.specificsystemdescription) cs ss "SpecSystDesc"
probDesc     cs ss = section' (titleize Doc.problemDescription) cs ss "ProbDesc"
termAndDefn  cs ss = section' (titleize' Doc.termAndDef)        cs ss "TermDefs"
termogy      cs ss = section' (titleize Doc.terminology)        cs ss "Terminology"
physSyst     cs ss = section (titleize Doc.physSyst)           cs ss physSystLabel
goalStmt     cs ss = section' (titleize' Doc.goalStmt)          cs ss "GoalStmt"
solCharSpec  cs ss = section (titleize Doc.solutionCharSpec)   cs ss solCharSpecLabel
thModel      cs ss = section (titleize' Doc.thModel)           cs ss thModelLabel
genDefn      cs ss = section (titleize' Doc.genDefn)           cs ss genDefnLabel
inModel      cs ss = section (titleize' Doc.inModel)           cs ss inModelLabel
dataDefn     cs ss = section (titleize' Doc.dataDefn)          cs ss dataDefnLabel
datCon       cs ss = section (titleize' Doc.datumConstraint)   cs ss datConLabel

propCorSol  cs ss = section' (titleize' Doc.propOfCorSol)      cs ss "CorSolProps"

require     cs ss = section' (titleize' Doc.requirement)      cs ss "Requirements"
nonfuncReq  cs ss = section (titleize' Doc.nonfunctionalRequirement) cs ss nonfuncReqLabel
funcReq     cs ss = section (titleize' Doc.functionalRequirement) cs ss funcReqLabel

likeChg     cs ss = section (titleize' Doc.likelyChg)        cs ss likeChgLabel
unlikeChg   cs ss = section (titleize' Doc.unlikelyChg)      cs ss unlikeChgLabel

traceyMandG   cs ss = section' (titleize' Doc.traceyMandG)    cs ss "TraceMatrices"
valsOfAuxCons cs ss = section  (titleize Doc.consVals)        cs ss valsOfAuxConsLabel
appendix      cs ss = section' (titleize Doc.appendix)        cs ss "Appendix"

offShelfSol cs ss = section' (titleize' Doc.offShelfSolution) cs ss "ExistingSolns"

--Root SRS Domain
srsDom :: CommonConcept
srsDom = dcc' "srsDom" (Doc.srs ^. term) "srs" ""

chgProbDom :: ConceptChunk
chgProbDom = ccs (nc "chgProbDom" $ cn' "change") EmptyS [srsDom]

likeChgDom :: ConceptChunk
likeChgDom = ccs (mkIdea "likeChgDom" (Doc.likelyChg ^. term) $ Just "LC") EmptyS [chgProbDom]

unlikeChgDom :: ConceptChunk
unlikeChgDom = ccs (mkIdea "unlikeChgDom" (Doc.unlikelyChg ^. term) $ Just "UC") EmptyS [chgProbDom]

assumpDom :: ConceptChunk
assumpDom = ccs (mkIdea "assumpDom" (Doc.assumption ^. term) $ Just "A") EmptyS [srsDom]

--Labels--
--FIXME: create using section information somehow?
physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, valsOfAuxConsLabel,
  indPRCaseLabel, unlikeChgLabel, funcReqLabel, nonfuncReqLabel,
  solCharSpecLabel :: Reference
physSystLabel      = makeSecRef "PhysSyst" "Physical System Description"
datConLabel        = makeSecRef "DataConstraints" "Data Constraints"
genDefnLabel       = makeSecRef "GDs" "General Definitions"
thModelLabel       = makeSecRef "TMs" "Theoretical Models"
dataDefnLabel      = makeSecRef "DDs" "Data Definitions"
inModelLabel       = makeSecRef "IMs" "Instance Models"
likeChgLabel       = makeSecRef "LCs" "Likely Changes"
unlikeChgLabel     = makeSecRef "UCs" "Unlikely Changes"
valsOfAuxConsLabel = makeSecRef "AuxConstants" "Values of Auxiliary Constants" --DO NOT CHANGE OR THINGS WILL BREAK -- see Language.Drasil.Document.Extract
indPRCaseLabel     = makeSecRef "IndividualProdUC" "Individual Product Use Cases"
funcReqLabel       = makeSecRef "FRs" "Functional Requirements"
nonfuncReqLabel    = makeSecRef "NFRs" "Nonfunctional Requirements"
solCharSpecLabel   = makeSecRef "SolCharSpec" "Solution Characteristics Specification"
