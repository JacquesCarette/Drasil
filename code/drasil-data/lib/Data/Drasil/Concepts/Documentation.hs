-- | Defines concepts used to create documentation.

-- Changes to documentation-related named chunks and common ideas should be reflected in the
-- 'Creating Your Project in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module Data.Drasil.Concepts.Documentation where

import Control.Lens ((^.))

import Language.Drasil hiding (organization, year, label, variable)
import Language.Drasil.Development (NPStruct)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.Metadata.Domains (softEng, documentc)
import qualified Drasil.Metadata.Documentation as Doc
import Drasil.Metadata.Documentation
  (softwareReq, specification, notebook)
import Drasil.Metadata.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

-- | Collects all documentation-related named chunks (not concept-level yet).
doccon :: [IdeaDict]
doccon = [abbAcc, abbreviation, acronym, analysis, appendix, aspect, body,
  caseProb, charOfIR, characteristic, class_, client, code, column, company,
  component, concept, condition, connection, consVals, constant, constraint,
  consumer, content, context, coordinate, coordinateSystem, corSol, customer,
  datum, datumConstraint, decision, definition, dependency, description,
  design, designDoc, document, documentation, effect, element, emphasis,
  endUser, environment, example, failure, figure, first, form, full,
  fullForm, functional, functionalRequirement, game, general,
  generalSystemDescription, goal, guide, implementation, indPRCase,
  individual, information, input_, instance_, intReader, interest, interface,
  introduction, issue, item, label, library, limitation, literacy, loss,
  material_, mainIdea, message, method_, methAndAnls, model, module_, name_,
  nonfunctional, nonfunctionalRequirement, notApp, object, offShelf,
  offShelfSolution, open, orgOfDoc, organization, output_, physical,
  physicalConstraint, physicalProperty, physicalSim, physicalSystem, physics,
  plan, practice, priority, problem, problemDescription, problemIntro,
  procedure, prodUCTable, productUC, product_, project, procForAnls,
  propOfCorSol, property, prpsOfDoc, purpose, quantity, realtime, review,
  reference, refMat, reqInput, response, result, reviewer,
  safety, safetyReq, scenario, scope, scpOfReq, scpOfTheProjS, second_,
  section_, simulation, software, softwareConstraint, softwareDoc,
  softwareReq, softwareSys, softwareVAV, softwareVerif, solution,
  solutionCharSpec, solutionCharacteristic, summary, source, specific,
  specification, specificsystemdescription, stakeholder, standard, statement,
  symbol_, sysCont, system, systemConstraint, systemdescription, tAuxConsts,
  tOfCont, tOfSymb, tOfUnit, inDatumConstraint, outDatumConstraint, table_,
  task, template, termAndDef, term_, terminology, theory, traceyGraph,
  traceyMandG, traceyMatrix, type_, uncertCol, uncertainty, useCase,
  useCaseTable, user, userCharacteristic, userInput, validation, value,
  variable, vav, vavPlan, verification, video, year]

-- | Collects all documentation-related common ideas (like a concept, but with no definition).
doccon' :: [CI]
doccon' = [assumption, dataConst, dataDefn, desSpec, genDefn, goalStmt, inModel,
  likelyChg, learnObj, mg, mis, notebook, physSyst, refBy, requirement, srs,
  thModel, typUnc, unlikelyChg]

assumption, desSpec, goalStmt, dataConst, likelyChg, learnObj, unlikelyChg, physSyst,
  mg, mis, typUnc, sec, refBy, refName,
  -- re-exports
  requirement, srs :: CI

-- * Common Ideas

------------------------------------------------------------------------------------------------------------------------------
-- | CI       |                  |    uid      |         term                                   | abbreviation | ConceptDomain
------------------------------------------------------------------------------------------------------------------------------
assumption  = Doc.assumption
desSpec     = Doc.desSpec
goalStmt    = Doc.goalStmt
dataConst   = commonIdeaWithDict "dataConst"   (cn' "data constraint")                               "DC"      [softEng]
learnObj    = Doc.learnObj
likelyChg   = Doc.likelyChg
unlikelyChg = Doc.unlikelyChg
physSyst    = Doc.physSyst
mis         = Doc.mis
mg          = Doc.mg
typUnc      = Doc.typUnc
sec         = Doc.sec
refBy       = Doc.refBy
refName     = commonIdeaWithDict "refName"     (cn' "reference name")                                "Refname" [documentc]
requirement = Doc.requirement
srs         = Doc.srs

---------------------------------------------------------------------

-- concepts relating to the templates and their contents
-- * Named Chunks
-- ** Basic Chunks

abbreviation, acronym, analysis, appendix, aspect, body, characteristic, class_, client,
  code, column, company, component, concept, condition, connection, constant,
  constraint, consumer, content, context, coordinate, customer, datum, decision,
  definition, dependency, description, design, document, documentation, effect,
  element, emphasis, endUser, environment, example, failure, figure, first, form, full,
  functional, game, general, goal, guide, implementation, individual, information,
  interest, interface, input_, instance_, intReader, introduction, issue, item,
  loss, label, library, limitation, literacy, material_, mainIdea, message, method_, module_,
  model, name_, nonfunctional, notApp, object, offShelf, open, organization, output_,
  physics, physical, plan, practice, priority, problem, procedure, product_, project,
  property, purpose, quantity, realtime, review, reference, response,
  result, reviewer, safety, scope, scpOfTheProjS, second_, section_, scenario,
  source, simulation, solution, summary, specific, stakeholder,
  standard, statement, symbol_, system, table_, task, template, term_, terminology,
  theory, traceyGraph, traceyMatrix, type_, uncertainty, user, useCase, validation,
  value, variable, video, verification, year :: IdeaDict

abbreviation    = Doc.abbreviation
acronym         = Doc.acronym
analysis        = nc "analysis"       (cnIS   "analysis"           )
appendix        = Doc.appendix
aspect          = nc "aspect"         (cn'    "aspect"             )
body            = nc "body"           (cnIES  "body"               )
characteristic  = Doc.characteristic
class_          = nc "class"          (cn'''  "class"              )
client          = Doc.client
code            = Doc.code
column          = Doc.column
company         = nc "company"        (cnIES  "company"            )
component       = Doc.component
concept         = nc "concept"        (cn'    "concept"            )
condition       = nc "condition"      (cn'    "condition"          )
connection      = nc "connection"     (cn'    "connection"         )
constant        = nc "constant"       (cn'    "constant"           )
constraint      = nc "constraint"     (cn'    "constraint"         )
consumer        = nc "consumer"       (cn'    "consumer"           )
content         = Doc.content
context         = Doc.context
coordinate      = nc "coordinate"     (cn'    "coordinate"         )
customer        = Doc.customer
datum           = Doc.datum
decision        = Doc.decision
definition      = Doc.definition
dependency      = Doc.dependency
description     = Doc.description
design          = Doc.design
document        = nc "document"       (cn'    "document"           )
documentation   = Doc.documentation
effect          = nc "effect"         (cn'    "effect"             )
element         = Doc.element
emphasis        = nc "emphasis"       (cnIS   "emphasis"           )
endUser         = Doc.endUser
environment     = Doc.environment
example         = Doc.example
failure         = nc "failure"        (cn'    "failure"            )
figure          = nc "figure"         (cn'    "figure"             )
first           = nc "first"          (cn'    "first"              ) --Does it make sense for this to be here?
form            = Doc.form
full            = Doc.full
functional      = Doc.functional
game            = nc "game"           (cn'    "game"               )
general         = Doc.general
goal            = Doc.goal
guide           = Doc.guide
implementation  = nc "implementation" (cn'    "implementation"     )
individual      = Doc.individual
information     = Doc.information
interest        = Doc.interest
interface       = Doc.interface
input_          = Doc.input_
instance_       = nc "instance"       (cn'    "instance"           )
intReader       = Doc.intReader
introduction    = Doc.introduction
issue           = nc "issue"          (cn'    "issue"              )
item            = Doc.item
label           = nc "label"          (cn'    "label"              )
library         = nc "library"        (cnIES  "library"            )
limitation      = Doc.limitation
literacy        = nc "literacy"       (cnIES  "literacy"           )
loss            = nc "loss"           (cn'''  "loss"               )
material_       = nc "material"       (cn'    "material"           )
mainIdea        = nc "mainIdea"       (cn'    "main idea"          )
message         = nc "message"        (cn'    "message"            )
method_         = nc "method"         (cn'    "method"             )
module_         = Doc.module_
model           = Doc.model
name_           = nc "name"           (cn'    "name"               )
nonfunctional   = Doc.nonfunctional
object          = nc "object"         (cn'    "object"             )
offShelf        = Doc.offShelf
open            = nc "open"           (cn'    "open"               )
organization    = Doc.organization
output_         = Doc.output_
physics         = nc "physics"        (cn'    "physics"            )
physical        = nc "physical"       (cn'    "physical"           ) --FIXME: Adjective
plan            = Doc.plan
practice        = nc "practice"       (cn'    "practice"           )
priority        = nc "priority"       (cnIES  "priority"           )
problem         = Doc.problem
procedure       = nc "procedure"      (cn'    "procedure"          )
product_        = Doc.product_
project         = Doc.project
property        = Doc.property
purpose         = Doc.purpose
quantity        = Doc.quantity
realtime        = nc "real-time"      (cn'    "real-time"          )
review          = Doc.review
reference       = Doc.reference
response        = nc "response"       (cn'    "response"           )
result          = nc "result"         (cn'    "result"             )
reviewer        = nc "reviewer"       (cn'    "reviewer"           )
safety          = nc "safety"         (cnIES  "safety"             )
scope           = nc "scope"          (cn'    "scope"              )
second_         = nc "second"         (cn'    "second"             ) --Does it make sense for this to be here?
section_        = Doc.section_
scenario        = nc "scenario"       (cn'    "scenario"           )
source          = nc "source"         (cn'    "source"             )
simulation      = nc "simulation"     (cn'    "simulation"         )
solution        = Doc.solution
summary         = Doc.summary
specific        = Doc.specific
stakeholder     = Doc.stakeholder
standard        = nc "standard"       (cn'    "standard"           )
statement       = Doc.statement
symbol_         = Doc.symbol_
system          = Doc.system
table_          = Doc.table_
task            = nc "task"           (cn'    "task"               )
template        = nc "template"       (cn'    "template"           )
term_           = Doc.term_
terminology     = Doc.terminology
theory          = Doc.theory
traceyGraph     = Doc.traceyGraph
traceyMatrix    = Doc.traceyMatrix
type_           = nc "type"           (cn'    "type"               )
uncertainty     = Doc.uncertainty
user            = Doc.user
useCase         = Doc.useCase
validation      = Doc.validation
value           = Doc.value
variable        = Doc.variable
verification    = Doc.verification
video           = nc "video"          (cn'    "video"              )
year            = nc "year"           (cn'    "year"               )
scpOfTheProjS   = nc "scpOfTheProj"   (cn'    "scope of the project") -- temporary generated for test

notApp          = mkIdea "notApp" (nounPhraseSP "not applicable")   (Just "N/A")

abbAcc, caseProb, charOfIR, consVals, corSol, methAndAnls, orgOfDoc, procForAnls, propOfCorSol, prpsOfDoc,
  refMat, reqInput, scpOfReq, tAuxConsts, tOfSymb, tOfUnit,
  termAndDef, traceyMandG, vav, tOfCont :: IdeaDict

abbAcc              = Doc.abbAcc
caseProb            = Doc.caseProb
consVals            = Doc.consVals
corSol              = Doc.corSol
charOfIR            = Doc.charOfIR
inDatumConstraint   = Doc.inDatumConstraint
methAndAnls         = nc "methAndAnls"        (method_ `and_` analysis)
orgOfDoc            = Doc.orgOfDoc
outDatumConstraint  = Doc.outDatumConstraint
procForAnls         = nc "procForAnls"        (procedure `for` analysis)
propOfCorSol        = Doc.propOfCorSol
prpsOfDoc           = Doc.prpsOfDoc
refMat              = nc "refMat"             (cn' "reference material")
reqInput            = Doc.reqInput
scpOfReq            = Doc.scpOfReq
tAuxConsts          = nc "TAuxConsts"         (cn' "auxiliary constant")
termAndDef          = Doc.termAndDef
tOfCont             = Doc.tOfCont
tOfSymb             = Doc.tOfSymb
tOfUnit             = Doc.tOfUnit
traceyMandG         = Doc.traceyMandG
vav                 = Doc.vav

scpOfTheProj :: (IdeaDict -> NPStruct) -> IdeaDict
scpOfTheProj = Doc.scpOfTheProj

-- ** Compound Chunks

designDoc, fullForm, generalSystemDescription, moduleInterface, indPRCase,
  physicalConstraint, physicalSystem, problemDescription, problemIntro, prodUCTable,
  specificsystemdescription, systemdescription, systemConstraint, sysCont,
  userCharacteristic, coordinateSystem, datumConstraint, inDatumConstraint,
  outDatumConstraint, functionalRequirement, nonfunctionalRequirement, safetyReq,
  software, softwareConstraint, softwareDoc, softwareSys, softwareVerif,
  softwareVAV, solutionCharSpec, solutionCharacteristic, offShelfSolution,
  physicalSim, productUC, useCaseTable, physicalProperty, vavPlan, uncertCol, userInput :: IdeaDict

coordinateSystem             = compoundNC coordinate system
datumConstraint              = Doc.datumConstraint
designDoc                    = compoundNC design document
fullForm                     = Doc.fullForm
functionalRequirement        = Doc.functionalRequirement
generalSystemDescription     = Doc.generalSystemDescription
moduleInterface              = Doc.moduleInterface
indPRCase                    = Doc.indPRCase
--inDatumConstraint            = compoundNC input_ datumConstraint -- may be used later, but they break stable for now
--outDatumConstraint           = compoundNC output_ datumConstraint
nonfunctionalRequirement     = Doc.nonfunctionalRequirement
offShelfSolution             = Doc.offShelfSolution
physicalConstraint           = Doc.physicalConstraint
physicalProperty             = compoundNC physical property
physicalSim                  = compoundNC physical simulation
physicalSystem               = Doc.physicalSystem
problemDescription           = Doc.problemDescription
problemIntro                 = compoundNC problem introduction
prodUCTable                  = Doc.prodUCTable
productUC                    = Doc.productUC
safetyReq                    = compoundNC safety requirement
softwareConstraint           = Doc.softwareConstraint
softwareDoc                  = Doc.softwareDoc
softwareSys                  = compoundNC software system
softwareVAV                  = Doc.softwareVAV
softwareVerif                = compoundNC software verification
solutionCharSpec             = Doc.solutionCharSpec
solutionCharacteristic       = Doc.solutionCharacteristic
specificsystemdescription    = Doc.specificsystemdescription
sysCont                      = Doc.sysCont
systemConstraint             = compoundNC system constraint
systemdescription            = Doc.systemdescription
uncertCol                    = compoundNC uncertainty column
useCaseTable                 = compoundNC useCase table_
userCharacteristic           = Doc.userCharacteristic
userInput                    = compoundNC user input_
vavPlan                      = Doc.vavPlan
software                     = Doc.software

-- * Domains


goalStmtDom, assumpDom, reqDom, funcReqDom, nonFuncReqDom, chgProbDom,
  likeChgDom, unlikeChgDom, refByDom, refNameDom :: ConceptChunk
assumpDom     = Doc.assumpDom
chgProbDom    = Doc.chgProbDom
funcReqDom    = Doc.funcReqDom
goalStmtDom   = Doc.goalStmtDom
likeChgDom    = Doc.likeChgDom
nonFuncReqDom = Doc.nonFuncReqDom
refByDom      = ccs (mkIdea "refByDom"      (refBy ^. term)                    $ Just "RefBy")    EmptyS [Doc.srsDom]
refNameDom    = ccs (mkIdea "refNameDom"    (refName ^. term)                  $ Just "RefName")  EmptyS [Doc.srsDom]
reqDom        = Doc.reqDom
unlikeChgDom  = Doc.unlikeChgDom

-- | List of SRS-related concepts, including SRS.
srsDomains :: [ConceptChunk]
srsDomains = [cw Doc.srsDom, goalStmtDom, reqDom, funcReqDom, nonFuncReqDom,
  assumpDom, chgProbDom, likeChgDom, unlikeChgDom, refByDom, refNameDom]
