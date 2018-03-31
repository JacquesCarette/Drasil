module Drasil.SRS
 (doc, doc', intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assump, thModel,
  genDefn, inModel, dataDefn, datCon, require, nonfuncReq, funcReq, likeChg, traceyMandG,
  appendix, reference, propCorSol, offShelfSol, missingP, valsOfAuxCons, tOfSymb) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc

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
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assump, thModel,
  genDefn, inModel, dataDefn, datCon, propCorSol, require, nonfuncReq, funcReq, likeChg, traceyMandG, tOfSymb,
  appendix, reference, offShelfSol, valsOfAuxCons :: [Contents] -> [Section] -> Section

intro       cs ss = section (titleize Doc.introduction) cs ss "Intro"
prpsOfDoc   cs ss = section (titleize Doc.prpsOfDoc) cs ss "DocPurpose"
scpOfReq    cs ss = section (titleize Doc.scpOfReq)  cs ss "ReqsScope"
charOfIR    cs ss = section (titleize' Doc.charOfIR) cs ss "ReaderChars"
orgOfDoc    cs ss = section (titleize Doc.orgOfDoc)  cs ss "DocOrg"

stakeholder cs ss = section (titleize' Doc.stakeholder) cs ss "Stakeholder"
theCustomer cs ss = section (titleize $ the Doc.customer) cs ss "Customer"
theClient   cs ss = section (titleize $ the Doc.client) cs ss "Client"

genSysDes   cs ss = section (titleize Doc.generalSystemDescription) cs ss "GenSysDesc"
sysCont     cs ss = section (titleize Doc.sysCont)              cs ss  "SysContext"
userChar    cs ss = section (titleize' Doc.userCharacteristic)  cs ss  "UserChars"
sysCon      cs ss = section (titleize' Doc.systemConstraint)    cs ss  "SysConstraints"

scpOfTheProj cs ss = section (at_start (Doc.scpOfTheProj titleize)) cs ss "ProjScope"
prodUCTable cs ss = section (titleize Doc.prodUCTable)      cs ss      "UseCaseTable"
indPRCase   cs ss = section (titleize' Doc.indPRCase)       cs ss      "IndividualProdUC"

specSysDes  cs ss = section (titleize Doc.specificsystemdescription) cs ss "SpecSystDesc"
probDesc    cs ss = section (titleize Doc.problemDescription) cs ss "ProbDesc"
termAndDefn cs ss = section (titleize' Doc.termAndDef)        cs ss "TermDefs"
termogy     cs ss = section (titleize Doc.terminology)        cs ss "Terminology"
physSyst    cs ss = section (titleize Doc.physSyst)           cs ss "PhysSyst"
goalStmt    cs ss = section (titleize' Doc.goalStmt)          cs ss "GoalStmt"
solCharSpec cs ss = section (titleize Doc.solutionCharSpec)   cs ss "SolCharSpec"
assump      cs ss = section (titleize' Doc.assumption)        cs ss "Assumps"
thModel     cs ss = section (titleize' Doc.thModel)           cs ss "TMs"
genDefn     cs ss = section (titleize' Doc.genDefn)           cs ss "GDs"
inModel     cs ss = section (titleize' Doc.inModel)           cs ss "IMs"
dataDefn    cs ss = section (titleize' Doc.dataDefn)          cs ss "DDs"
datCon      cs ss = section (titleize' Doc.datumConstraint)   cs ss "DataConstraints"

propCorSol  cs ss = section (titleize' Doc.propOfCorSol)      cs ss "CorSolProps"

require     cs ss = section (titleize' Doc.requirement)      cs ss "Requirements"
nonfuncReq  cs ss = section (titleize' Doc.nonfunctionalRequirement) cs ss "NFRs"
funcReq     cs ss = section (titleize' Doc.functionalRequirement) cs ss "FRs"

likeChg     cs ss = section (titleize' Doc.likelyChg)        cs ss "LCs"

traceyMandG cs ss = section (titleize' Doc.traceyMandG)      cs ss "TraceMatrices"

valsOfAuxCons cs ss = section (titleize Doc.consVals)        cs ss "AuxConstants"

appendix    cs ss = section (titleize Doc.appendix)          cs ss "Appendix"

reference   cs ss = section (titleize' Doc.reference)        cs ss "References"
offShelfSol cs ss = section (titleize' Doc.offShelfSolution) cs ss "ExistingSolns"

tOfSymb cs ss = section (titleize Doc.tOfSymb) cs ss "ToS"

--
missingP :: [Contents]
missingP = [Paragraph $ S "..."]
