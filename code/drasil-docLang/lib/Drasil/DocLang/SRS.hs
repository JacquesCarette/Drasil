-- | Holds all section constructors and labels for creating SRS documents.

-- Changes to SRS sections should be reflected in the 'Creating Your Project
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module Drasil.DocLang.SRS (
  -- * Section Constructors
  -- | For use in an SRS document. Ordered by appearance in a SRS.
  tOfCont, refMat, tOfUnit, tOfSymb, tOfAbbAcc, intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, genSysDes, sysCont,
  userChar, sysCon, specSysDes, probDesc, termAndDefn, physSyst, goalStmt,
  solCharSpec, assumpt, thModel, genDefn, dataDefn, inModel, datCon, propCorSol,
  require, nonfuncReq, funcReq, likeChg, unlikeChg, traceyMandG, valsOfAuxCons,
  reference, appendix, offShelfSol, scpOfTheProj, prodUCTable, indPRCase,
  termogy,
  -- * Section Labels
  -- | Labels linked to the associated section constructor. Ordered by appearance in a SRS.
  tOfContLabel, refMatLabel, tOfUnitLabel, tOfSymbLabel, tOfAbbAccLabel,
  introLabel, docPurposeLabel, reqsScopeLabel, readerCharsLabel, docOrgLabel,
  stakeholderLabel, clientLabel, customerLabel, genSysDescLabel, sysContextLabel,
  userCharsLabel, sysConstraintsLabel, specSystDescLabel, physSystLabel, probDescLabel,
  termDefsLabel, goalStmtLabel, solCharSpecLabel, assumptLabel, thModelLabel,
  genDefnLabel, dataDefnLabel, inModelLabel, datConLabel, corSolPropsLabel, requirementsLabel,
  funcReqLabel, nonfuncReqLabel, likeChgLabel, unlikeChgLabel, traceMatricesLabel,
  valsOfAuxConsLabel, referenceLabel, appendixLabel, offShelfSolnsLabel, indPRCaseLabel,
  projScopeLabel, useCaseTableLabel, terminologyLabel,
  -- * All Section References
  sectionReferences) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D

import qualified Data.Drasil.Concepts.Documentation as Doc (appendix, assumption,
  charOfIR, client, customer, consVals, datumConstraint, functionalRequirement,
  generalSystemDescription, goalStmt, indPRCase, introduction, likelyChg,
  unlikelyChg, nonfunctionalRequirement, offShelfSolution, orgOfDoc, physSyst,
  prodUCTable, problemDescription, propOfCorSol, prpsOfDoc, reference,
  scpOfReq, scpOfTheProj, solutionCharSpec, specificsystemdescription,
  stakeholder, sysCont, systemConstraint, termAndDef, terminology, traceyMandG,
  tOfCont, tOfSymb, tOfUnit, userCharacteristic, refMat, abbAcc)
import qualified Drasil.Metadata as M (dataDefn, genDefn, inModel, thModel, requirement)

import Control.Lens ((^.), view)

-- Ordered by appearance in SRS.
-- | Standard SRS section builders.
tOfCont, refMat, tOfUnit, tOfSymb, tOfAbbAcc, intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, genSysDes, sysCont,
  userChar, sysCon, specSysDes, probDesc, termAndDefn, physSyst, goalStmt,
  solCharSpec, assumpt, thModel, genDefn, dataDefn, inModel, datCon, propCorSol,
  require, nonfuncReq, funcReq, likeChg, unlikeChg, traceyMandG, valsOfAuxCons,
  reference, appendix, offShelfSol, scpOfTheProj, prodUCTable, indPRCase,
  termogy :: [Contents] -> [Section] -> Section

-- | Table of Contents section.
tOfCont       cs ss = section (titleize' Doc.tOfCont)                  cs ss tOfContLabel

-- | Reference Material section.
refMat        cs ss = section (titleize Doc.refMat)                    cs ss refMatLabel
-- | Table of Units section.
tOfUnit       cs ss = section (titleize' Doc.tOfUnit)                  cs ss tOfUnitLabel
-- | Table of Symbols section.
tOfSymb       cs ss = section (titleize' Doc.tOfSymb)                  cs ss tOfSymbLabel
-- | Table of Abbreviations and Acronyms section.
tOfAbbAcc     cs ss = section (titleize' Doc.abbAcc)                   cs ss tOfAbbAccLabel

-- | Introduction section.
intro         cs ss = section (titleize Doc.introduction)              cs ss introLabel
-- | Purpose of Document section.
prpsOfDoc     cs ss = section (titleize Doc.prpsOfDoc)                 cs ss docPurposeLabel
-- | Scope of Requirements section.
scpOfReq      cs ss = section (titleize' Doc.scpOfReq)                 cs ss reqsScopeLabel
-- | Characteristics of Intended Reader section.
charOfIR      cs ss = section (titleize' Doc.charOfIR)                 cs ss readerCharsLabel
-- | Organization of Document section.
orgOfDoc      cs ss = section (titleize Doc.orgOfDoc)                  cs ss docOrgLabel

-- | Stakeholders section.
stakeholder   cs ss = section (titleize' Doc.stakeholder)              cs ss stakeholderLabel
-- | The Customer section.
theCustomer   cs ss = section (D.toSent $ titleizeNP $ the Doc.customer) cs ss customerLabel
-- | The Client section.
theClient     cs ss = section (D.toSent $ titleizeNP $ the Doc.client) cs ss clientLabel

-- | General System Description section.
genSysDes     cs ss = section (titleize Doc.generalSystemDescription)  cs ss genSysDescLabel
-- | System Context section.
sysCont       cs ss = section (titleize Doc.sysCont)                   cs ss sysContextLabel
-- | User Characteristics section.
userChar      cs ss = section (titleize' Doc.userCharacteristic)       cs ss userCharsLabel
-- | System Constraints section.
sysCon        cs ss = section (titleize' Doc.systemConstraint)         cs ss sysConstraintsLabel

-- | Specific System Description section.
specSysDes    cs ss = section (titleize Doc.specificsystemdescription) cs ss specSystDescLabel

-- | Problem Description section.
probDesc      cs ss = section (titleize Doc.problemDescription)        cs ss probDescLabel
-- | Terminology and Definitions section.
termAndDefn   cs ss = section (titleize' Doc.termAndDef)               cs ss termDefsLabel
-- | Physical System Description section.
physSyst      cs ss = section (titleize Doc.physSyst)                  cs ss physSystLabel
-- | Goal Statement section.
goalStmt      cs ss = section (titleize' Doc.goalStmt)                 cs ss goalStmtLabel

-- | Solution Characteristics Specification section.
solCharSpec   cs ss = section (titleize Doc.solutionCharSpec)          cs ss solCharSpecLabel
-- | Assumptions section.
assumpt       cs ss = section (titleize' Doc.assumption)               cs ss assumptLabel
-- | Theoretical Models section.
thModel       cs ss = section (titleize' M.thModel)                    cs ss thModelLabel
-- | General Definitions section.
genDefn       cs ss = section (titleize' M.genDefn)                    cs ss genDefnLabel
-- | Data Definitions section.
dataDefn      cs ss = section (titleize' M.dataDefn)                   cs ss dataDefnLabel
-- | Instance Models section.
inModel       cs ss = section (titleize' M.inModel)                    cs ss inModelLabel
-- | Data Constraints section.
datCon        cs ss = section (titleize' Doc.datumConstraint)          cs ss datConLabel
-- | Properties of a Correct Solution section.
propCorSol    cs ss = section (titleize' Doc.propOfCorSol)             cs ss corSolPropsLabel

-- | Requirements section.
require       cs ss = section (titleize' M.requirement)                cs ss requirementsLabel
-- | Non-Functional Requirements section.
nonfuncReq    cs ss = section (titleize' Doc.nonfunctionalRequirement) cs ss nonfuncReqLabel
-- | Functional Requirements section.
funcReq       cs ss = section (titleize' Doc.functionalRequirement)    cs ss funcReqLabel

-- | Likely Changes section.
likeChg       cs ss = section (titleize' Doc.likelyChg)                cs ss likeChgLabel
-- | Unlikely Changes section.
unlikeChg     cs ss = section (titleize' Doc.unlikelyChg)              cs ss unlikeChgLabel

-- | Traceablilty Matrices and Graphs section.
traceyMandG   cs ss = section (titleize' Doc.traceyMandG)              cs ss traceMatricesLabel
-- | Values of Auxiliary Constants section.
valsOfAuxCons cs ss = section (titleize Doc.consVals)                  cs ss valsOfAuxConsLabel
-- | References section.
reference     cs ss = section (titleize' Doc.reference)                cs ss referenceLabel
-- | Appendix section.
appendix      cs ss = section (titleize Doc.appendix)                  cs ss appendixLabel
-- | Off-the-Shelf Solutions section.
offShelfSol   cs ss = section (titleize' Doc.offShelfSolution)         cs ss offShelfSolnsLabel

-- Unused
-- | Scope of the Project section.
scpOfTheProj  cs ss = section (atStart (Doc.scpOfTheProj (\s -> titleizeNP (s ^. term))))    cs ss projScopeLabel
-- | Product Use Case Table section.
prodUCTable   cs ss = section (titleize Doc.prodUCTable)               cs ss useCaseTableLabel
-- | Individual Product Use Case section.
indPRCase     cs ss = section (titleize' Doc.indPRCase)                cs ss indPRCaseLabel
-- | Terminology section.
termogy       cs ss = section (titleize Doc.terminology)               cs ss terminologyLabel

--Labels--
-- | Collections all 'Section' 'Reference's.
sectionReferences :: [Reference]
sectionReferences = [tOfContLabel, refMatLabel, tOfUnitLabel, tOfSymbLabel, tOfAbbAccLabel,
  introLabel, docPurposeLabel, reqsScopeLabel, readerCharsLabel, docOrgLabel,
  stakeholderLabel, clientLabel, customerLabel, genSysDescLabel, sysContextLabel,
  userCharsLabel, sysConstraintsLabel, specSystDescLabel, physSystLabel, probDescLabel,
  termDefsLabel, goalStmtLabel, solCharSpecLabel, assumptLabel, thModelLabel,
  genDefnLabel, dataDefnLabel, inModelLabel, datConLabel, corSolPropsLabel, requirementsLabel,
  funcReqLabel, nonfuncReqLabel, likeChgLabel, unlikeChgLabel, traceMatricesLabel,
  valsOfAuxConsLabel, referenceLabel, appendixLabel, offShelfSolnsLabel, indPRCaseLabel,
  projScopeLabel, useCaseTableLabel, terminologyLabel]

--FIXME: create using section information somehow?
-- | Makes a 'Reference' to a 'Section'.
tOfContLabel, refMatLabel, tOfUnitLabel, tOfSymbLabel, tOfAbbAccLabel,
  introLabel, docPurposeLabel, reqsScopeLabel, readerCharsLabel, docOrgLabel,
  stakeholderLabel, clientLabel, customerLabel, genSysDescLabel, sysContextLabel,
  userCharsLabel, sysConstraintsLabel, specSystDescLabel, physSystLabel, probDescLabel,
  termDefsLabel, goalStmtLabel, solCharSpecLabel, assumptLabel, thModelLabel,
  genDefnLabel, dataDefnLabel, inModelLabel, datConLabel, corSolPropsLabel, requirementsLabel,
  funcReqLabel, nonfuncReqLabel, likeChgLabel, unlikeChgLabel, traceMatricesLabel,
  valsOfAuxConsLabel, referenceLabel, appendixLabel, offShelfSolnsLabel, indPRCaseLabel,
  projScopeLabel, useCaseTableLabel, terminologyLabel :: Reference

tOfContLabel        = makeSecRef "ToC"              $ titleize' Doc.tOfCont

refMatLabel         = makeSecRef "RefMat"           $ titleize  Doc.refMat
tOfUnitLabel        = makeSecRef "ToU"              $ titleize' Doc.tOfUnit
tOfSymbLabel        = makeSecRef "ToS"              $ titleize' Doc.tOfSymb
tOfAbbAccLabel      = makeSecRef "TAbbAcc"          $ titleize' Doc.abbAcc

introLabel          = makeSecRef "Intro"            $ titleize  Doc.introduction
docPurposeLabel     = makeSecRef "DocPurpose"       $ titleize  Doc.prpsOfDoc
reqsScopeLabel      = makeSecRef "ReqsScope"        $ titleize' Doc.scpOfReq
readerCharsLabel    = makeSecRef "ReaderChars"      $ titleize' Doc.charOfIR
docOrgLabel         = makeSecRef "DocOrg"           $ titleize  Doc.orgOfDoc

stakeholderLabel    = makeSecRef "Stakeholder"      $ titleize' Doc.stakeholder
clientLabel         = makeSecRef "Client"           $ D.toSent $ titleizeNP $ the Doc.client
customerLabel       = makeSecRef "Customer"         $ D.toSent $ titleizeNP $ the Doc.customer

genSysDescLabel     = makeSecRef "GenSysDesc"       $ titleize  Doc.generalSystemDescription
sysContextLabel     = makeSecRef "SysContext"       $ titleize  Doc.sysCont
userCharsLabel      = makeSecRef "UserChars"        $ titleize' Doc.userCharacteristic
sysConstraintsLabel = makeSecRef "SysConstraints"   $ titleize' Doc.systemConstraint

specSystDescLabel   = makeSecRef "SpecSystDesc"     $ titleize  Doc.specificsystemdescription
physSystLabel       = makeSecRef "PhysSyst"         $ titleize  Doc.physSyst
probDescLabel       = makeSecRef "ProbDesc"         $ titleize  Doc.problemDescription
termDefsLabel       = makeSecRef "TermDefs"         $ titleize' Doc.termAndDef
goalStmtLabel       = makeSecRef "GoalStmt"         $ titleize' Doc.goalStmt

solCharSpecLabel    = makeSecRef "SolCharSpec"      $ titleize  Doc.solutionCharSpec
assumptLabel        = makeSecRef "Assumps"          $ titleize' Doc.assumption
thModelLabel        = makeSecRef "TMs"              $ titleize' M.thModel
genDefnLabel        = makeSecRef "GDs"              $ titleize' M.genDefn
dataDefnLabel       = makeSecRef "DDs"              $ titleize' M.dataDefn
inModelLabel        = makeSecRef "IMs"              $ titleize' M.inModel
datConLabel         = makeSecRef "DataConstraints"  $ titleize' Doc.datumConstraint
corSolPropsLabel    = makeSecRef "CorSolProps"      $ titleize' Doc.propOfCorSol

requirementsLabel   = makeSecRef "Requirements"     $ titleize' M.requirement
funcReqLabel        = makeSecRef "FRs"              $ titleize' Doc.functionalRequirement
nonfuncReqLabel     = makeSecRef "NFRs"             $ titleize' Doc.nonfunctionalRequirement

likeChgLabel        = makeSecRef "LCs"              $ titleize' Doc.likelyChg
unlikeChgLabel      = makeSecRef "UCs"              $ titleize' Doc.unlikelyChg

traceMatricesLabel  = makeSecRef "TraceMatrices"    $ titleize' Doc.traceyMandG
valsOfAuxConsLabel  = makeSecRef "AuxConstants"     $ titleize  Doc.consVals
referenceLabel      = makeSecRef "References"       $ titleize' Doc.reference
appendixLabel       = makeSecRef "Appendix"         $ titleize  Doc.appendix
offShelfSolnsLabel  = makeSecRef "offShelfSolns"    $ titleize' Doc.offShelfSolution

-- Used only under People/Dan/Presentations/CommitteeMeeting4/BodyNew.hs
indPRCaseLabel      = makeSecRef "IndividualProdUC" $ titleize' Doc.indPRCase
-- Seem to be unused. Should they be deleted?
projScopeLabel      = makeSecRef "ProjScope"        $ atStart $ Doc.scpOfTheProj (titleizeNP . view term)
useCaseTableLabel   = makeSecRef "UseCaseTable"     $ titleize  Doc.prodUCTable
terminologyLabel    = makeSecRef "Terminology"      $ titleize  Doc.terminology
