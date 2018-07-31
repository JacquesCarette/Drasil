module Drasil.DocLang.SRS
 (doc, doc', intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient,
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assumpt, thModel,
  genDefn, inModel, dataDefn, datCon, require, nonfuncReq, funcReq, likeChg, unlikeChg, 
  traceyMandG, appendix, reference, propCorSol, offShelfSol, valsOfAuxCons,
  physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, tOfSymbLabel, valsOfAuxConsLabel, referenceLabel,
  indPRCaseLabel, unlikeChgLabel, assumptLabel, funcReqLabel,
  tOfSymb, srsDom) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil
import qualified Data.Drasil.Concepts.Documentation as Doc (appendix, 
    assumption, charOfIR, client, customer, consVals, dataDefn, datumConstraint, 
    functionalRequirement, genDefn, generalSystemDescription, goalStmt, 
    indPRCase, inModel, introduction, likelyChg, unlikelyChg, nonfunctionalRequirement,
    offShelfSolution, orgOfDoc, physSyst, prodUCTable, problemDescription, 
    propOfCorSol, prpsOfDoc, reference, requirement, scpOfReq, scpOfTheProj,
    solutionCharSpec, specificsystemdescription, srs, stakeholder, sysCont, 
    systemConstraint, termAndDef, terminology, thModel, traceyMandG, tOfSymb, 
    userCharacteristic)
import Data.Drasil.Phrase (for'')

import Control.Lens ((^.))

-- Local function to keep things looking clean, not exported.
forTT :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT = for'' titleize' titleize

forTT' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT' = for'' titleize' titleize'

-- | SRS document constructor. 
-- Create the SRS from given system name, authors, and sections
doc, doc' :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc sys authors secs = Document (Doc.srs `forTT` sys) authors secs
-- | Uses plural of system for title.
doc' sys authors secs = Document (Doc.srs `forTT'` sys) authors secs

-- | Standard SRS section builders
intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assumpt, thModel,
  genDefn, inModel, dataDefn, datCon, propCorSol, require, nonfuncReq, funcReq, likeChg, traceyMandG, tOfSymb,
  appendix, reference, offShelfSol, valsOfAuxCons, unlikeChg :: [Contents] -> [Section] -> Section

intro       cs ss = section' (titleize Doc.introduction) cs ss "Intro"
prpsOfDoc   cs ss = section' (titleize Doc.prpsOfDoc) cs ss "DocPurpose"
scpOfReq    cs ss = section' (titleize Doc.scpOfReq)  cs ss "ReqsScope"
charOfIR    cs ss = section' (titleize' Doc.charOfIR) cs ss "ReaderChars"
orgOfDoc    cs ss = section' (titleize Doc.orgOfDoc)  cs ss "DocOrg"

stakeholder cs ss = section' (titleize' Doc.stakeholder) cs ss "Stakeholder"
theCustomer cs ss = section' (titleize $ the Doc.customer) cs ss "Customer"
theClient   cs ss = section' (titleize $ the Doc.client) cs ss "Client"

genSysDes   cs ss = section' (titleize Doc.generalSystemDescription) cs ss "GenSysDesc"
sysCont     cs ss = section' (titleize Doc.sysCont)              cs ss  "SysContext"
userChar    cs ss = section' (titleize' Doc.userCharacteristic)  cs ss  "UserChars"
sysCon      cs ss = section' (titleize' Doc.systemConstraint)    cs ss  "SysConstraints"

scpOfTheProj cs ss = section' (at_start (Doc.scpOfTheProj titleize)) cs ss "ProjScope"
prodUCTable cs ss = section' (titleize Doc.prodUCTable)      cs ss      "UseCaseTable"
indPRCase   cs ss = section' (titleize' Doc.indPRCase)       cs ss      "IndividualProdUC" --FIXME: label is available

specSysDes  cs ss = section' (titleize Doc.specificsystemdescription) cs ss "SpecSystDesc"
probDesc    cs ss = section' (titleize Doc.problemDescription) cs ss "ProbDesc"
termAndDefn cs ss = section' (titleize' Doc.termAndDef)        cs ss "TermDefs"
termogy     cs ss = section' (titleize Doc.terminology)        cs ss "Terminology"
physSyst    cs ss = section' (titleize Doc.physSyst)           cs ss "PhysSyst" --FIXME: label is available
goalStmt    cs ss = section' (titleize' Doc.goalStmt)          cs ss "GoalStmt"
solCharSpec cs ss = section' (titleize Doc.solutionCharSpec)   cs ss "SolCharSpec"
assumpt     cs ss = section' (titleize' Doc.assumption)        cs ss "Assumps" --FIXME: label is available
thModel     cs ss = section' (titleize' Doc.thModel)           cs ss "TMs" --FIXME: label is available
genDefn     cs ss = section' (titleize' Doc.genDefn)           cs ss "GDs" --FIXME: label is available
inModel     cs ss = section' (titleize' Doc.inModel)           cs ss "IMs" --FIXME: label is available
dataDefn    cs ss = section' (titleize' Doc.dataDefn)          cs ss "DDs" --FIXME: label is available
datCon      cs ss = section' (titleize' Doc.datumConstraint)   cs ss "DataConstraints" --FIXME: label is available

propCorSol  cs ss = section' (titleize' Doc.propOfCorSol)      cs ss "CorSolProps"

require     cs ss = section' (titleize' Doc.requirement)      cs ss "Requirements"
nonfuncReq  cs ss = section' (titleize' Doc.nonfunctionalRequirement) cs ss "NFRs"
funcReq     cs ss = section' (titleize' Doc.functionalRequirement) cs ss "FRs" --FIXME: label is available

likeChg     cs ss = section' (titleize' Doc.likelyChg)        cs ss "LCs" --FIXME: label is available
unlikeChg   cs ss = section' (titleize' Doc.unlikelyChg)      cs ss "UCs" --FIXME: label is available

traceyMandG cs ss = section' (titleize' Doc.traceyMandG)      cs ss "TraceMatrices"

valsOfAuxCons cs ss = section' (titleize Doc.consVals)        cs ss "AuxConstants" --FIXME: label is available

appendix    cs ss = section' (titleize Doc.appendix)          cs ss "Appendix"

reference   cs ss = section' (titleize' Doc.reference)        cs ss "References" --FIXME: label is available
offShelfSol cs ss = section' (titleize' Doc.offShelfSolution) cs ss "ExistingSolns"

tOfSymb cs ss = section' (titleize Doc.tOfSymb) cs ss "ToS" --FIXME: label is available

--Root SRS Domain
srsDom :: CommonConcept
srsDom = dcc' "srsDom" (Doc.srs ^. term) "srs" ""

--function that sets the shortname of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> String -> Section
section' a b c d = section a b c (mkLabelRA'' d)

--Labels--
physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, tOfSymbLabel, valsOfAuxConsLabel, referenceLabel,
  indPRCaseLabel, unlikeChgLabel, assumptLabel, funcReqLabel :: Label
physSystLabel      = mkLabelRA   "PhysSyst" "PhysSyst" "Physical System Description"
datConLabel        = mkLabelRA'' "DataConstraints"
genDefnLabel       = mkLabelRA'' "GDs"
thModelLabel       = mkLabelRA'' "TMs"
dataDefnLabel      = mkLabelRA   "DDs" "DDs" "Data Definitions"
inModelLabel       = mkLabelRA'' "IMs"
likeChgLabel       = mkLabelRA'' "LCs"
unlikeChgLabel     = mkLabelRA'' "UCs"
tOfSymbLabel       = mkLabelRA'' "ToS"
valsOfAuxConsLabel = mkLabelRA   "AuxConstants" "AuxConstants" "Values of Auxiliary Constants"
referenceLabel     = mkLabelRA'' "References" 
indPRCaseLabel     = mkLabelRA   "IndividualProdUC" "IndividualProdUC" "Individual Product Use Cases"
assumptLabel       = mkLabelRA   "Assumps" "Assumps" "Assumptions"
funcReqLabel       = mkLabelRA   "FRs" "FRs" "Functional Requirements"
