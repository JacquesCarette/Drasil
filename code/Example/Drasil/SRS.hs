module Drasil.SRS
 (doc, doc', intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assump, thModel,
  genDefn, inModel, dataDefn, datCon, require, nonfuncReq, funcReq, likeChg, traceyMandG,
  appendix, reference, propCorSol, offShelfSol, missingP) where
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
  genDefn, inModel, dataDefn, datCon, propCorSol, require, nonfuncReq, funcReq, likeChg, traceyMandG,
  appendix, reference, offShelfSol :: [Contents] -> [Section] -> Section

intro       = section (Doc.fterm titleize Doc.introduction)
prpsOfDoc   = section (Doc.fterm titleize Doc.prpsOfDoc)
scpOfReq    = section (Doc.fterm titleize Doc.scpOfReq)
charOfIR    = section (Doc.fterm titleize' Doc.charOfIR)
orgOfDoc    = section (Doc.fterm titleize Doc.orgOfDoc)

stakeholder = section (Doc.fterm titleize' Doc.stakeholder)
theCustomer = section (Doc.fterm titleize $ the Doc.customer)
theClient   = section (Doc.fterm titleize $ the Doc.client)

genSysDes   = section (Doc.fterm titleize Doc.generalSystemDescription)
sysCont     = section (Doc.fterm titleize Doc.sysCont)
userChar    = section (Doc.fterm titleize' Doc.userCharacteristic)
sysCon      = section (Doc.fterm titleize' Doc.systemConstraint)

scpOfTheProj = section (Doc.fterm at_start (Doc.scpOfTheProj titleize))
prodUCTable = section (Doc.fterm titleize Doc.prodUCTable)
indPRCase   = section (Doc.fterm titleize' Doc.indPRCase)

specSysDes  = section (Doc.fterm titleize Doc.specificsystemdescription)
probDesc    = section (Doc.fterm titleize Doc.problemDescription)
termAndDefn = section (Doc.fterm titleize' Doc.termAndDef)
termogy     = section (Doc.fterm titleize Doc.terminology)
physSyst    = section (Doc.fterm titleize Doc.physSyst)
goalStmt    = section (Doc.fterm titleize' Doc.goalStmt)
solCharSpec = section (Doc.fterm titleize' Doc.solutionCharSpec)
assump      = section (Doc.fterm titleize' Doc.assumption)
thModel     = section (Doc.fterm titleize' Doc.thModel)
genDefn     = section (Doc.fterm titleize' Doc.genDefn)
inModel     = section (Doc.fterm titleize' Doc.inModel)
dataDefn    = section (Doc.fterm titleize' Doc.dataDefn)
datCon      = section (Doc.fterm titleize' Doc.datumConstraint)

propCorSol  = section (Doc.fterm titleize' Doc.propOfCorSol)

require     = section (Doc.fterm titleize' Doc.requirement)
nonfuncReq  = section (Doc.fterm titleize' Doc.nonfunctionalRequirement)
funcReq     = section (Doc.fterm titleize' Doc.functionalRequirement)

likeChg     = section (Doc.fterm titleize' Doc.likelyChg)

traceyMandG = section (Doc.fterm titleize' Doc.traceyMandG)

appendix    = section (Doc.fterm titleize Doc.appendix)

reference   = section (Doc.fterm titleize' Doc.reference)
offShelfSol = section (Doc.fterm titleize' Doc.offShelfSolution)

--
missingP :: [Contents]
missingP = [Paragraph Doc.missing]
