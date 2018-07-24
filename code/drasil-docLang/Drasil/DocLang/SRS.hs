module Drasil.DocLang.SRS
 (doc, doc', intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assumpt, thModel,
  genDefn, inModel, dataDefn, datCon, require, nonfuncReq, funcReq, likeChg, unlikeChg, 
  traceyMandG, appendix, reference, propCorSol, offShelfSol, valsOfAuxCons,
  tOfSymb, 
  physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, inModelLabel,
  likeChgLabel, unlikeChgLabel, valsOfAuxConsLabel, referenceLabel, indPRCaseLabel) where
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
  genSysDes, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, goalStmt, solCharSpec, require, 
  nonfuncReq, reference, offShelfSol :: [Contents] -> [Section] -> Section

intro       cs ss = section' (titleize Doc.introduction) cs ss "Intro"
prpsOfDoc   cs ss = section' (titleize Doc.prpsOfDoc) cs ss "DocPurpose"
scpOfReq    cs ss = section' (titleize Doc.scpOfReq)  cs ss "ReqsScope"
charOfIR    cs ss = section' (titleize' Doc.charOfIR) cs ss "ReaderChars"
orgOfDoc    cs ss = section' (titleize Doc.orgOfDoc)  cs ss "DocOrg"

stakeholder cs ss = section' (titleize' Doc.stakeholder) cs ss "Stakeholder"
theCustomer cs ss = section' (titleize $ the Doc.customer) cs ss "Customer"
theClient   cs ss = section' (titleize $ the Doc.client) cs ss "Client"

genSysDes   cs ss = section' (titleize Doc.generalSystemDescription) cs ss "GenSysDesc"
userChar    cs ss = section' (titleize' Doc.userCharacteristic)  cs ss  "UserChars"
sysCon      cs ss = section' (titleize' Doc.systemConstraint)    cs ss  "SysConstraints"

scpOfTheProj cs ss = section' (at_start (Doc.scpOfTheProj titleize)) cs ss "ProjScope"
prodUCTable cs ss = section' (titleize Doc.prodUCTable)      cs ss      "UseCaseTable"
indPRCase   cs ss = section' (titleize' Doc.indPRCase)       cs ss      "IndividualProdUC" --FIXME: label is available

specSysDes  cs ss = section' (titleize Doc.specificsystemdescription) cs ss "SpecSystDesc"
probDesc    cs ss = section' (titleize Doc.problemDescription) cs ss "ProbDesc"
termAndDefn cs ss = section' (titleize' Doc.termAndDef)        cs ss "TermDefs"
termogy     cs ss = section' (titleize Doc.terminology)        cs ss "Terminology"
goalStmt    cs ss = section' (titleize' Doc.goalStmt)          cs ss "GoalStmt"
solCharSpec cs ss = section' (titleize Doc.solutionCharSpec)   cs ss "SolCharSpec"


require     cs ss = section' (titleize' Doc.requirement)      cs ss "Requirements"
nonfuncReq  cs ss = section' (titleize' Doc.nonfunctionalRequirement) cs ss "NFRs"

reference   cs ss = section' (titleize' Doc.reference)        cs ss "References" --FIXME: label is available
offShelfSol cs ss = section' (titleize' Doc.offShelfSolution) cs ss "ExistingSolns"



appendix, datCon, funcReq, genDefn, likeChg, physSyst, sysCont, traceyMandG,
 unlikeChg, tOfSymb, valsOfAuxCons, assumpt, dataDefn, inModel, thModel,
 propCorSol :: [LabelledContent] -> [Section] -> Section

appendix      cs ss = sectionLC (titleize Doc.appendix)               cs ss (mkLabelRA'' "Appendix")
assumpt       cs ss = sectionLC (titleize' Doc.assumption)            cs ss (mkLabelRA'' "Assumps")
datCon        cs ss = sectionLC (titleize' Doc.datumConstraint)       cs ss datConLabel
dataDefn      cs ss = sectionLC (titleize' Doc.dataDefn)              cs ss dataDefnLabel
funcReq       cs ss = sectionLC (titleize' Doc.functionalRequirement) cs ss (mkLabelRA'' "FRs")
genDefn       cs ss = sectionLC (titleize' Doc.genDefn)               cs ss genDefnLabel
inModel       cs ss = sectionLC (titleize' Doc.inModel)               cs ss inModelLabel
likeChg       cs ss = sectionLC (titleize' Doc.likelyChg)             cs ss likeChgLabel
physSyst      cs ss = sectionLC (titleize Doc.physSyst)               cs ss physSystLabel
propCorSol    cs ss = sectionLC (titleize' Doc.propOfCorSol)          cs ss (mkLabelRA'' "CorSolProps")
sysCont       cs ss = sectionLC (titleize Doc.sysCont)                cs ss (mkLabelRA'' "SysContext")
thModel       cs ss = sectionLC (titleize' Doc.thModel)               cs ss thModelLabel
tOfSymb       cs ss = sectionLC (titleize Doc.tOfSymb)                cs ss tOfSymbLabel
traceyMandG   cs ss = sectionLC (titleize' Doc.traceyMandG)           cs ss (mkLabelRA'' "TraceMatrices")
unlikeChg     cs ss = sectionLC (titleize' Doc.unlikelyChg)           cs ss unlikeChgLabel
valsOfAuxCons cs ss = sectionLC (titleize Doc.consVals)               cs ss valsOfAuxConsLabel

--function that sets the shortname of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> RefAdd -> Section
section' a b c d = section a b c (mkLabelRA' (d ++ "Label") d)

--Labels--
physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, tOfSymbLabel, valsOfAuxConsLabel, referenceLabel,
  indPRCaseLabel :: Label
physSystLabel      = mkLabelRA'' "PhysSyst"
datConLabel        = mkLabelRA'' "DataConstraints"
genDefnLabel       = mkLabelRA'' "GDs"
thModelLabel       = mkLabelRA'' "TMs"
dataDefnLabel      = mkLabelRA'' "DDs"
inModelLabel       = mkLabelRA'' "IMs"
likeChgLabel       = mkLabelRA'' "LCs"
unlikeChgLabel     = mkLabelRA'' "UCs"
tOfSymbLabel       = mkLabelRA'' "ToS"
valsOfAuxConsLabel = mkLabelRA'' "AuxConstants"
referenceLabel     = mkLabelRA'' "References"
indPRCaseLabel     = mkLabelRA'' "IndividualProdUC"