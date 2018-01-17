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

intro       cs ss = section (titleize Doc.introduction) cs ss (S "Intro")
prpsOfDoc   cs ss = section (titleize Doc.prpsOfDoc) cs ss (S "DocPurpose")
scpOfReq    cs ss = section (titleize Doc.scpOfReq)  cs ss (S "ReqsScope")
charOfIR    cs ss = section (titleize' Doc.charOfIR) cs ss (S "ReaderChars")
orgOfDoc    cs ss = section (titleize Doc.orgOfDoc)  cs ss (S "DocOrg")

stakeholder cs ss = section (titleize' Doc.stakeholder) cs ss (S "Stakeholder")
theCustomer cs ss = section (titleize $ the Doc.customer) cs ss (S "Customer")
theClient   cs ss = section (titleize $ the Doc.client) cs ss (S "Client")

genSysDes   cs ss = section (titleize Doc.generalSystemDescription) cs ss (S "GenSysDesc")
sysCont     cs ss = section (titleize Doc.sysCont)              cs ss  (S "SysContext")
userChar    cs ss = section (titleize' Doc.userCharacteristic)  cs ss  (S "UserChars")
sysCon      cs ss = section (titleize' Doc.systemConstraint)    cs ss  (S "SysConstraints")

scpOfTheProj cs ss = section (at_start (Doc.scpOfTheProj titleize)) cs ss (S "ProjScope")
prodUCTable cs ss = section (titleize Doc.prodUCTable)      cs ss      (S "UseCaseTable")
indPRCase   cs ss = section (titleize' Doc.indPRCase)       cs ss      (S "IndividualProdUC")

specSysDes  cs ss = section (titleize Doc.specificsystemdescription) cs ss (S "SpecSystDesc")
probDesc    cs ss = section (titleize Doc.problemDescription) cs ss (S "ProbDesc")
termAndDefn cs ss = section (titleize' Doc.termAndDef)        cs ss (S "TermDefs")
termogy     cs ss = section (titleize Doc.terminology)        cs ss (S "Terminology")
physSyst    cs ss = section (titleize Doc.physSyst)           cs ss (S "PhysSyst")
goalStmt    cs ss = section (titleize' Doc.goalStmt)          cs ss (S "GoalStmt")
solCharSpec cs ss = section (titleize Doc.solutionCharSpec)   cs ss (S "SolCharSpec")
assump      cs ss = section (titleize' Doc.assumption)        cs ss (S "Assumps")
thModel     cs ss = section (titleize' Doc.thModel)           cs ss (S "TMs")
genDefn     cs ss = section (titleize' Doc.genDefn)           cs ss (S "GDs")
inModel     cs ss = section (titleize' Doc.inModel)           cs ss (S "IMs")
dataDefn    cs ss = section (titleize' Doc.dataDefn)          cs ss (S "DDs")
datCon      cs ss = section (titleize' Doc.datumConstraint)   cs ss (S "DataConstraints")

propCorSol  cs ss = section (titleize' Doc.propOfCorSol)      cs ss (S "CorSolProps")

require     cs ss = section (titleize' Doc.requirement) cs ss (S "Requirements")
nonfuncReq  cs ss = section (titleize' Doc.nonfunctionalRequirement) cs ss (S "NFRs")
funcReq     cs ss = section (titleize' Doc.functionalRequirement) cs ss   (S "FRs")

likeChg     cs ss = section (titleize' Doc.likelyChg)  cs ss      (S "LCs")

traceyMandG cs ss = section (titleize' Doc.traceyMandG)   cs ss   (S "TraceMatrices")

valsOfAuxCons cs ss = section (titleize Doc.consVals)   cs ss     (S "AuxConstants")

appendix    cs ss = section (titleize Doc.appendix)    cs ss      (S "Appendix")

reference   cs ss = section (titleize' Doc.reference)    cs ss    (S "References")
offShelfSol cs ss = section (titleize' Doc.offShelfSolution) cs ss (S "ExistingSolns")

tOfSymb cs ss = section (titleize Doc.tOfSymb) cs ss (S "ToS")

--
missingP :: [Contents]
missingP = [Paragraph Doc.missing]
