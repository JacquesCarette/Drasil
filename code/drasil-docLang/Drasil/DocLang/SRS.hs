module Drasil.DocLang.SRS (appendix, assumpt, assumptLabel, charOfIR, datCon,
  datConLabel, dataDefn, dataDefnLabel, doc, doc', funcReq, funcReqLabel, genDefn,
  genDefnLabel, genSysDes, goalStmt, inModel, inModelLabel, indPRCase,
  indPRCaseLabel, intro, likeChg, likeChgLabel, nonfuncReq, offShelfSol, orgOfDoc,
  physSyst, physSystLabel, probDesc, prodUCTable, propCorSol, prpsOfDoc, reference,
  referenceLabel, require, scpOfReq, scpOfTheProj, solCharSpec, solCharSpecLabel,
  specSysDes, stakeholder, sysCon, sysCont, tOfSymb, tOfSymbLabel, tOfUnit,
  termAndDefn, termogy, thModel, thModelLabel, theClient, theCustomer, traceyMandG,
  unlikeChg, unlikeChgLabel, userChar, valsOfAuxCons, valsOfAuxConsLabel, sectionReferences) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S (forTPS, forTPP)

import qualified Data.Drasil.Concepts.Documentation as Doc (appendix, assumption,
  charOfIR, client, customer, consVals, datumConstraint, functionalRequirement,
  generalSystemDescription, goalStmt, indPRCase, introduction, likelyChg,
  unlikelyChg, nonfunctionalRequirement, offShelfSolution, orgOfDoc, physSyst,
  prodUCTable, problemDescription, propOfCorSol, prpsOfDoc, reference, requirement,
  scpOfReq, scpOfTheProj, solutionCharSpec, specificsystemdescription, srs,
  stakeholder, sysCont, systemConstraint, termAndDef, terminology, traceyMandG,
  tOfSymb, tOfUnit, userCharacteristic)
import qualified Data.Drasil.TheoryConcepts as Doc (dataDefn, genDefn, inModel, thModel)


-- | SRS document constructor. 
-- Creates the SRS from a given system name, authors, and sections.
doc, doc' :: NamedIdea c => c -> Sentence -> [Section] -> Document
-- | Names document "Software Requirement Specifications for @system@".
doc  sys = Document (Doc.srs `S.forTPS` sys)
-- | Similar to 'doc', but uses plural form of @systems@ in the title.
doc' sys = Document (Doc.srs `S.forTPP` sys)

-- | Standard SRS section builders.
intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, 
  genSysDes, sysCont, userChar, sysCon, scpOfTheProj, prodUCTable, indPRCase, specSysDes,
  probDesc, termAndDefn, termogy, physSyst, goalStmt, solCharSpec, assumpt, thModel,
  genDefn, inModel, dataDefn, datCon, propCorSol, require, nonfuncReq, funcReq, likeChg,
  unlikeChg, traceyMandG, valsOfAuxCons, appendix, reference, offShelfSol, tOfSymb,
  tOfUnit :: [Contents] -> [Section] -> Section

-- | Introduction section.
intro     cs ss = section' (titleize Doc.introduction) cs ss "Intro"
-- | Purpose of Document section.
prpsOfDoc cs ss = section' (titleize Doc.prpsOfDoc)    cs ss "DocPurpose"
-- | Scope of Requirements section.
scpOfReq  cs ss = section' (titleize Doc.scpOfReq)     cs ss "ReqsScope"
-- | Characteristics of Intended Reader section.
charOfIR  cs ss = section' (titleize' Doc.charOfIR)    cs ss "ReaderChars"
-- | Organization of Document section.
orgOfDoc  cs ss = section' (titleize Doc.orgOfDoc)     cs ss "DocOrg"

-- | Stakeholders section.
stakeholder cs ss = section' (titleize' Doc.stakeholder)      cs ss "Stakeholder"
-- | The Customer section.
theCustomer cs ss = section' (titleizeNP $ theT Doc.customer) cs ss "Customer"
-- | The Client section.
theClient   cs ss = section' (titleizeNP $ theT Doc.client)   cs ss "Client"

-- | General System Description section.
genSysDes cs ss = section' (titleize Doc.generalSystemDescription) cs ss "GenSysDesc"
-- | System Context section.
sysCont   cs ss = section' (titleize Doc.sysCont)                  cs ss "SysContext"
-- | User Characteristics section.
userChar  cs ss = section' (titleize' Doc.userCharacteristic)      cs ss "UserChars"
-- | System Constraints section.
sysCon    cs ss = section' (titleize' Doc.systemConstraint)        cs ss "SysConstraints"

-- | Scope of the Project section.
scpOfTheProj cs ss = section' (atStart (Doc.scpOfTheProj titleize)) cs ss "ProjScope"
-- | Product Use Case Table section.
prodUCTable  cs ss = section' (titleize Doc.prodUCTable)            cs ss "UseCaseTable"
-- | Individual Product Use Case section.
indPRCase    cs ss = section  (titleize' Doc.indPRCase)             cs ss indPRCaseLabel

-- | Specific System Description section.
specSysDes  cs ss = section' (titleize Doc.specificsystemdescription) cs ss "SpecSystDesc"
-- | Problem Description section.
probDesc    cs ss = section' (titleize Doc.problemDescription)        cs ss "ProbDesc"
-- | Terminology and Definitions section.
termAndDefn cs ss = section' (titleize' Doc.termAndDef)               cs ss "TermDefs"
-- | Terminology section.
termogy     cs ss = section' (titleize Doc.terminology)               cs ss "Terminology"
-- | Physical System Description section.
physSyst    cs ss = section  (titleize Doc.physSyst)                  cs ss physSystLabel
-- | Goal Statement section.
goalStmt    cs ss = section' (titleize' Doc.goalStmt)                 cs ss "GoalStmt"
-- | Solution Characteristics Specification section.
solCharSpec cs ss = section  (titleize Doc.solutionCharSpec)          cs ss solCharSpecLabel
-- | Assumptions section.
assumpt     cs ss = section  (titleize' Doc.assumption)               cs ss assumptLabel
-- | Theoretical Models section.
thModel     cs ss = section  (titleize' Doc.thModel)                  cs ss thModelLabel
-- | General Definitions section.
genDefn     cs ss = section  (titleize' Doc.genDefn)                  cs ss genDefnLabel
-- | Instance Models section.
inModel     cs ss = section  (titleize' Doc.inModel)                  cs ss inModelLabel
-- | Data Definitions section.
dataDefn    cs ss = section  (titleize' Doc.dataDefn)                 cs ss dataDefnLabel
-- | Data Constraints section.
datCon      cs ss = section  (titleize' Doc.datumConstraint)          cs ss datConLabel

-- | Properties of a Correct Solution section.
propCorSol cs ss = section' (titleize' Doc.propOfCorSol) cs ss "CorSolProps"

-- | Requirements section.
require    cs ss = section' (titleize' Doc.requirement)              cs ss "Requirements"
-- | Non-Functional Requirements section.
nonfuncReq cs ss = section  (titleize' Doc.nonfunctionalRequirement) cs ss nonfuncReqLabel
-- | Functional Requirements section.
funcReq    cs ss = section  (titleize' Doc.functionalRequirement)    cs ss funcReqLabel

-- | Likely Changes section.
likeChg   cs ss = section (titleize' Doc.likelyChg)   cs ss likeChgLabel
-- | Unlikely Changes section.
unlikeChg cs ss = section (titleize' Doc.unlikelyChg) cs ss unlikeChgLabel

-- | Traceablilty Matrices and Graphs section.
traceyMandG   cs ss = section' (titleize' Doc.traceyMandG) cs ss "TraceMatrices"
-- | Values of Auxiliary Constants section.
valsOfAuxCons cs ss = section  (titleize Doc.consVals)     cs ss valsOfAuxConsLabel
-- | Appendix section.
appendix      cs ss = section' (titleize Doc.appendix)     cs ss "Appendix"

-- | References section.
reference   cs ss = section  (titleize' Doc.reference)        cs ss referenceLabel
-- | Off-the-Shelf Solutions section.
offShelfSol cs ss = section' (titleize' Doc.offShelfSolution) cs ss "offShelfSolns"

-- | Table of Symbols section.
tOfSymb cs ss = section  (titleize Doc.tOfSymb) cs ss tOfSymbLabel
-- | Table of Units section.
tOfUnit cs ss = section' (titleize Doc.tOfUnit) cs ss "ToU"

--function that sets the shortname of each section to be the reference address
--FIXME: should avoid using toString
-- | Similar to 'section'. Takes a title ('Sentence'), introductory contents 
-- (ie. paragraphs, tables, etc.), a list of subsections, and a name for the section. 
-- However, the title also becomes the shortname.
section' :: Sentence -> [Contents] -> [Section] -> String -> Section
section' a b c d = section a b c (makeSecRef d (toString a))
  where
    toString :: Sentence -> String --FIXME: same as sentenceDoc, import instead? 
    toString (S x) = x
    toString ((:+:) s1 s2) = toString s1 ++ toString s2
    toString _ = error "Term is not a string"

--Labels--
-- | Collections all 'Section' 'Reference's.
sectionReferences :: [Reference]
sectionReferences = [physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, tOfSymbLabel, valsOfAuxConsLabel, referenceLabel,
  indPRCaseLabel, unlikeChgLabel, assumptLabel, funcReqLabel, nonfuncReqLabel,
  solCharSpecLabel]

--FIXME: create using section information somehow?
-- | Makes a 'Reference' to a 'Section'.
physSystLabel, datConLabel, genDefnLabel, thModelLabel, dataDefnLabel, 
  inModelLabel, likeChgLabel, tOfSymbLabel, valsOfAuxConsLabel, referenceLabel,
  indPRCaseLabel, unlikeChgLabel, assumptLabel, funcReqLabel, nonfuncReqLabel,
  solCharSpecLabel :: Reference
physSystLabel      = makeSecRef "PhysSyst" "Physical System Description"
datConLabel        = makeSecRef "DataConstraints" "Data Constraints"
genDefnLabel       = makeSecRef "GDs" "General Definitions"
thModelLabel       = makeSecRef "TMs" "Theoretical Models"
dataDefnLabel      = makeSecRef "DDs" "Data Definitions"
inModelLabel       = makeSecRef "IMs" "Instance Models"
likeChgLabel       = makeSecRef "LCs" "Likely Changes"
unlikeChgLabel     = makeSecRef "UCs" "Unlikely Changes"
tOfSymbLabel       = makeSecRef "ToS" "Table of Symbols"
valsOfAuxConsLabel = makeSecRef "AuxConstants" "Values of Auxiliary Constants" --DO NOT CHANGE OR THINGS WILL BREAK -- see Language.Drasil.Document.Extract
referenceLabel     = makeSecRef "References" "References" 
indPRCaseLabel     = makeSecRef "IndividualProdUC" "Individual Product Use Cases"
assumptLabel       = makeSecRef "Assumps" "Assumptions"
funcReqLabel       = makeSecRef "FRs" "Functional Requirements"
nonfuncReqLabel    = makeSecRef "NFRs" "Nonfunctional Requirements"
solCharSpecLabel   = makeSecRef "SolCharSpec" "Solution Characteristics Specification"
