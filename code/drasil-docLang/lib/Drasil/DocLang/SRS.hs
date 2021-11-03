-- | Holds all section constructors and labels for creating SRS documents.
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
import Utils.Drasil.Concepts

import qualified Data.Drasil.Concepts.Documentation as Doc (appendix, assumption,
  charOfIR, client, customer, consVals, datumConstraint, functionalRequirement,
  generalSystemDescription, goalStmt, indPRCase, introduction, likelyChg,
  unlikelyChg, nonfunctionalRequirement, offShelfSolution, orgOfDoc, physSyst,
  prodUCTable, problemDescription, propOfCorSol, prpsOfDoc, reference, requirement,
  scpOfReq, scpOfTheProj, solutionCharSpec, specificsystemdescription,
  stakeholder, sysCont, systemConstraint, termAndDef, terminology, traceyMandG,
  tOfCont, tOfSymb, tOfUnit, userCharacteristic, refMat, abbAcc)
import qualified Data.Drasil.TheoryConcepts as Doc (dataDefn, genDefn, inModel, thModel)


-- Ordered by appearance in SRS.
-- | Standard SRS section builders.
tOfCont, refMat, tOfUnit, tOfSymb, tOfAbbAcc, intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, genSysDes, sysCont,
  userChar, sysCon, specSysDes, probDesc, termAndDefn, physSyst, goalStmt,
  solCharSpec, assumpt, thModel, genDefn, dataDefn, inModel, datCon, propCorSol,
  require, nonfuncReq, funcReq, likeChg, unlikeChg, traceyMandG, valsOfAuxCons,
  reference, appendix, offShelfSol, scpOfTheProj, prodUCTable, indPRCase,
  termogy :: Int -> [Contents] -> [Section] -> Section

-- | Table of Contents section.
tOfCont       d cs ss = section d (titleize' Doc.tOfCont)                  cs ss tOfContLabel

-- | Reference Material section.
refMat        d cs ss = section d (titleize Doc.refMat)                    cs ss refMatLabel
-- | Table of Units section.
tOfUnit       d cs ss = section d (titleize' Doc.tOfUnit)                  cs ss tOfUnitLabel
-- | Table of Symbols section.
tOfSymb       d cs ss = section d (titleize' Doc.tOfSymb)                  cs ss tOfSymbLabel
-- | Table of Abbreviations and Acronyms section.
tOfAbbAcc     d cs ss = section d (titleize' Doc.abbAcc)                   cs ss tOfAbbAccLabel

-- | Introduction section.
intro         d cs ss = section d (titleize Doc.introduction)              cs ss introLabel
-- | Purpose of Document section.
prpsOfDoc     d cs ss = section d (titleize Doc.prpsOfDoc)                 cs ss docPurposeLabel
-- | Scope of Requirements section.
scpOfReq      d cs ss = section d (titleize' Doc.scpOfReq)                 cs ss reqsScopeLabel
-- | Characteristics of Intended Reader section.
charOfIR      d cs ss = section d (titleize' Doc.charOfIR)                 cs ss readerCharsLabel
-- | Organization of Document section.
orgOfDoc      d cs ss = section d (titleize Doc.orgOfDoc)                  cs ss docOrgLabel

-- | Stakeholders section.
stakeholder   d cs ss = section d (titleize' Doc.stakeholder)              cs ss stakeholderLabel
-- | The Customer section.
theCustomer   d cs ss = section d (titleizeNP $ the Doc.customer)          cs ss customerLabel
-- | The Client section.
theClient     d cs ss = section d (titleizeNP $ the Doc.client)            cs ss clientLabel

-- | General System Description section.
genSysDes     d cs ss = section d (titleize Doc.generalSystemDescription)  cs ss genSysDescLabel
-- | System Context section.
sysCont       d cs ss = section d (titleize Doc.sysCont)                   cs ss sysContextLabel
-- | User Characteristics section.
userChar      d cs ss = section d (titleize' Doc.userCharacteristic)       cs ss userCharsLabel
-- | System Constraints section.
sysCon        d cs ss = section d (titleize' Doc.systemConstraint)         cs ss sysConstraintsLabel

-- | Specific System Description section.
specSysDes    d cs ss = section d (titleize Doc.specificsystemdescription) cs ss specSystDescLabel

-- | Problem Description section.
probDesc      d cs ss = section d (titleize Doc.problemDescription)        cs ss probDescLabel
-- | Terminology and Definitions section.
termAndDefn   d cs ss = section d (titleize' Doc.termAndDef)               cs ss termDefsLabel
-- | Physical System Description section.
physSyst      d cs ss = section d (titleize Doc.physSyst)                  cs ss physSystLabel
-- | Goal Statement section.
goalStmt      d cs ss = section d (titleize' Doc.goalStmt)                 cs ss goalStmtLabel

-- | Solution Characteristics Specification section.
solCharSpec   d cs ss = section d (titleize Doc.solutionCharSpec)          cs ss solCharSpecLabel
-- | Assumptions section.
assumpt       d cs ss = section d (titleize' Doc.assumption)               cs ss assumptLabel
-- | Theoretical Models section.
thModel       d cs ss = section d (titleize' Doc.thModel)                  cs ss thModelLabel
-- | General Definitions section.
genDefn       d cs ss = section d (titleize' Doc.genDefn)                  cs ss genDefnLabel
-- | Data Definitions section.
dataDefn      d cs ss = section d (titleize' Doc.dataDefn)                 cs ss dataDefnLabel
-- | Instance Models section.
inModel       d cs ss = section d (titleize' Doc.inModel)                  cs ss inModelLabel
-- | Data Constraints section.
datCon        d cs ss = section d (titleize' Doc.datumConstraint)          cs ss datConLabel
-- | Properties of a Correct Solution section.
propCorSol    d cs ss = section d (titleize' Doc.propOfCorSol)             cs ss corSolPropsLabel

-- | Requirements section.
require       d cs ss = section d (titleize' Doc.requirement)              cs ss requirementsLabel
-- | Non-Functional Requirements section.
nonfuncReq    d cs ss = section d (titleize' Doc.nonfunctionalRequirement) cs ss nonfuncReqLabel
-- | Functional Requirements section.
funcReq       d cs ss = section d (titleize' Doc.functionalRequirement)    cs ss funcReqLabel

-- | Likely Changes section.
likeChg       d cs ss = section d (titleize' Doc.likelyChg)                cs ss likeChgLabel
-- | Unlikely Changes section.
unlikeChg     d cs ss = section d (titleize' Doc.unlikelyChg)              cs ss unlikeChgLabel

-- | Traceablilty Matrices and Graphs section.
traceyMandG   d cs ss = section d (titleize' Doc.traceyMandG)              cs ss traceMatricesLabel
-- | Values of Auxiliary Constants section.
valsOfAuxCons d cs ss = section d (titleize Doc.consVals)                  cs ss valsOfAuxConsLabel
-- | References section.
reference     d cs ss = section d (titleize' Doc.reference)                cs ss referenceLabel
-- | Appendix section.
appendix      d cs ss = section d (titleize Doc.appendix)                  cs ss appendixLabel
-- | Off-the-Shelf Solutions section.
offShelfSol   d cs ss = section d (titleize' Doc.offShelfSolution)         cs ss offShelfSolnsLabel

-- Unused
-- | Scope of the Project section.
scpOfTheProj  d cs ss = section d (atStart (Doc.scpOfTheProj titleize))      cs ss projScopeLabel
-- | Product Use Case Table section.
prodUCTable   d cs ss = section d (titleize Doc.prodUCTable)                 cs ss useCaseTableLabel
-- | Individual Product Use Case section.
indPRCase     d cs ss = section d (titleize' Doc.indPRCase)                  cs ss indPRCaseLabel
-- | Terminology section.
termogy       d cs ss = section d (titleize Doc.terminology)                 cs ss terminologyLabel

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
clientLabel         = makeSecRef "Client"           $ titleizeNP $ the Doc.client
customerLabel       = makeSecRef "Customer"         $ titleizeNP $ the Doc.customer

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
thModelLabel        = makeSecRef "TMs"              $ titleize' Doc.thModel
genDefnLabel        = makeSecRef "GDs"              $ titleize' Doc.genDefn
dataDefnLabel       = makeSecRef "DDs"              $ titleize' Doc.dataDefn
inModelLabel        = makeSecRef "IMs"              $ titleize' Doc.inModel
datConLabel         = makeSecRef "DataConstraints"  $ titleize' Doc.datumConstraint
corSolPropsLabel    = makeSecRef "CorSolProps"      $ titleize' Doc.propOfCorSol

requirementsLabel   = makeSecRef "Requirements"     $ titleize' Doc.requirement
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
projScopeLabel      = makeSecRef "ProjScope"        $ atStart $ Doc.scpOfTheProj titleize
useCaseTableLabel   = makeSecRef "UseCaseTable"     $ titleize  Doc.prodUCTable
terminologyLabel    = makeSecRef "Terminology"      $ titleize  Doc.terminology