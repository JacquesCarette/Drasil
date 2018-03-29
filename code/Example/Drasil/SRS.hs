module Drasil.SRS
 (doc, doc', intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assump, thModel,
  genDefn, inModel, dataDefn, datCon, require, nonfuncReq, funcReq, likeChg, traceyMandG,
  appendix, reference, propCorSol, offShelfSol, missingP, valsOfAuxCons) where
  	---zzzz A module in Haskell serves the dual purpose of controlling name-spaces and creating abstract data types. zzzz------
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc   --zzzz qualified means giving another name zzzz---

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
  appendix, reference, offShelfSol, valsOfAuxCons :: [Contents] -> [Section] -> Section

intro       = section (titleize Doc.introduction)
prpsOfDoc   = section (titleize Doc.prpsOfDoc)
scpOfReq    = section (titleize Doc.scpOfReq)
charOfIR    = section (titleize' Doc.charOfIR)
orgOfDoc    = section (titleize Doc.orgOfDoc)

stakeholder = section (titleize' Doc.stakeholder)
theCustomer = section (titleize $ the Doc.customer)
theClient   = section (titleize $ the Doc.client)

genSysDes   = section (titleize Doc.generalSystemDescription)
sysCont     = section (titleize Doc.sysCont)
userChar    = section (titleize' Doc.userCharacteristic)
sysCon      = section (titleize' Doc.systemConstraint)

scpOfTheProj = section (at_start (Doc.scpOfTheProj titleize))
prodUCTable = section (titleize Doc.prodUCTable)
indPRCase   = section (titleize' Doc.indPRCase)

specSysDes  = section (titleize Doc.specificsystemdescription)
probDesc    = section (titleize Doc.problemDescription)
termAndDefn = section (titleize' Doc.termAndDef)
termogy     = section (titleize Doc.terminology)
physSyst    = section (titleize Doc.physSyst)
goalStmt    = section (titleize' Doc.goalStmt)
solCharSpec = section (titleize Doc.solutionCharSpec)
assump      = section (titleize' Doc.assumption)
thModel     = section (titleize' Doc.thModel)
genDefn     = section (titleize' Doc.genDefn)
inModel     = section (titleize' Doc.inModel)
dataDefn    = section (titleize' Doc.dataDefn)
datCon      = section (titleize' Doc.datumConstraint)

propCorSol  = section (titleize' Doc.propOfCorSol)

require     = section (titleize' Doc.requirement)
nonfuncReq  = section (titleize' Doc.nonfunctionalRequirement)
funcReq     = section (titleize' Doc.functionalRequirement)

likeChg     = section (titleize' Doc.likelyChg)

traceyMandG = section (titleize' Doc.traceyMandG)

valsOfAuxCons = section (titleize Doc.consVals)

appendix    = section (titleize Doc.appendix)

reference   = section (titleize' Doc.reference)
offShelfSol = section (titleize' Doc.offShelfSolution)

--
missingP :: [Contents]
missingP = [Paragraph Doc.missing]
