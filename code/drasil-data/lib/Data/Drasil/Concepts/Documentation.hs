-- | Defines concepts used to create documentation.

-- Changes to documentation-related named chunks and common ideas should be reflected in the 
-- 'Creating Your Project in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil


module Data.Drasil.Concepts.Documentation where

import Language.Drasil hiding (organization, year, label, variable)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Math (graph, unit_)
import Data.Drasil.Domains (documentc, softEng)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Control.Lens ((^.))

-- | Collects all documentation-related named chunks (not concept-level yet).
doccon :: [NamedChunk]
doccon = [abbAcc, abbreviation, acronym, analysis, appendix, aspect, body, charOfIR, characteristic,
  class_, client, code, column, company, component, concept, condition, connection,
  consVals, constant, constraint, consumer, content, context, coordinate, coordinateSystem, 
  corSol, customer, datum, datumConstraint, decision, definition, dependency, description,
  design, designDoc, document, documentation, effect, element, emphasis, endUser,
  environment, example, failure, figure, first, form, full, fullForm, functional,
  functionalRequirement, game, general, generalSystemDescription, goal, guide, 
  implementation, indPRCase, individual, information, input_, instance_, intReader,
  interest, interface, introduction, issue, item, label, library, limitation,
  literacy, loss, material_, mainIdea, message, method_, methAndAnls, model, module_, name_, 
  nonfunctional, nonfunctionalRequirement, object, offShelf, offShelfSolution, open, orgOfDoc,
  organization, output_, physical, physicalConstraint, physicalProperty, physicalSim,
  physicalSystem, physics, plan, practice, priority, problem, problemDescription, procedure,
  prodUCTable, productUC, product_, project, procForAnls, propOfCorSol, property, prpsOfDoc,
  purpose, quantity, realtime, review, reference, refMat, reqInput, requirement_, response, result,
  reviewer, safety, safetyReq, scenario, scope, scpOfReq, scpOfTheProjS, second_,
  section_, simulation, software, softwareConstraint, softwareDoc, softwareReq,
  softwareSys, softwareVAV, softwareVerif, solution, solutionCharSpec,
  solutionCharacteristic, summary, source, specific, specification, specificsystemdescription,
  stakeholder, standard, statement, symbol_, sysCont, system, systemConstraint,
  systemdescription, tAuxConsts, tOfCont, tOfSymb, tOfUnit, inDatumConstraint, outDatumConstraint,
  table_, task, template, termAndDef, term_,
  terminology, theory, traceyGraph, traceyMandG, traceyMatrix, type_, uncertCol,
  uncertainty, useCase, useCaseTable, user, userCharacteristic, userInput,
  validation, value, variable, vav, vavPlan, verification, video, year]

-- | Collects all documentation-related common ideas (like a concept, but with no definition).
doccon' :: [CI]
doccon' = [assumption, dataConst, dataDefn, desSpec, genDefn, goalStmt, inModel,
  likelyChg, mg, mis, notApp, physSyst, requirement, srs, thModel, typUnc, unlikelyChg, notebook, refBy, refName]

assumption, desSpec, goalStmt, dataConst, likelyChg, unlikelyChg, physSyst, requirement, 
  mg, mis, notApp, srs, typUnc, sec, notebook, refBy, refName :: CI

softReqSpec :: NP
softReqSpec = fterms compoundPhraseP1 softwareReq specification

-- * Common Ideas

------------------------------------------------------------------------------------------------------------------------------
-- | CI       |                  |    uid      |         term                                   | abbreviation | ConceptDomain
------------------------------------------------------------------------------------------------------------------------------
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                    "A"       [softEng]
desSpec     = commonIdeaWithDict "desSpec"     (combineNINI design specification)                    "DS"      [softEng]
goalStmt    = commonIdeaWithDict "goalStmt"    (combineNINI goal statement)                          "GS"      [softEng]
dataConst   = commonIdeaWithDict "dataConst"   (cn' "data constraint")                               "DC"      [softEng]
likelyChg   = commonIdeaWithDict "likelyChg"   (cn' "likely change")                                 "LC"      [softEng]
unlikelyChg = commonIdeaWithDict "unlikelyChg" (cn' "unlikely change")                               "UC"      [softEng]
physSyst    = commonIdeaWithDict "physSyst"    (combineNINI physicalSystem description)              "PS"      [softEng]
requirement = commonIdeaWithDict "requirement" (cn' "requirement")                                   "R"       [softEng]
mis         = commonIdeaWithDict "mis"         (fterms compoundPhrase moduleInterface specification) "MIS"     [softEng]
mg          = commonIdeaWithDict "mg"          (fterms compoundPhrase module_ guide)                 "MG"      [softEng]
notApp      = commonIdea         "notApp"      (nounPhraseSP "not applicable")                       "N/A"     []
typUnc      = commonIdeaWithDict "typUnc"      (cn' "typical uncertainty")                           "Uncert." [softEng]
sec         = commonIdeaWithDict "section"     (cn' "section")                                       "Sec"     [documentc]
srs         = commonIdeaWithDict "srs"         softReqSpec                                           "SRS"     [softEng]
notebook    = commonIdeaWithDict "notebook"    (cn' "notebook")                                      "NB"      [softEng]
refBy       = commonIdeaWithDict "refBy"       (cn  "referenced by")                                 "RefBy"   [documentc]
refName     = commonIdeaWithDict "refName"     (cn' "reference name")                                "Refname" [documentc]

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
  model, name_, nonfunctional, object, offShelf, open, organization, output_,
  physics, physical, plan, practice, priority, problem, procedure, product_, project,
  property, purpose, quantity, realtime, review, reference, requirement_, response, 
  result, reviewer, safety, scope, scpOfTheProjS, second_, section_, scenario,
  source, simulation, software, solution, summary, specific, specification, stakeholder,
  standard, statement, symbol_, system, table_, task, template, term_, terminology,
  theory, traceyGraph, traceyMatrix, type_, uncertainty, user, useCase, validation,
  value, variable, video, verification, year :: NamedChunk

abbreviation    = nc "abbreviation"   (cn'    "abbreviation"       )
acronym         = nc "acronym"        (cn'    "acronym"            )
analysis        = nc "analysis"       (cnIS   "analysis"           )
appendix        = nc "appendix"       (cnICES "appendix"           )
aspect          = nc "aspect"         (cn'    "aspect"             )
body            = nc "body"           (cnIES  "body"               )
characteristic  = nc "characteristic" (cn'    "characteristic"     )
class_          = nc "class"          (cn'''  "class"              )
client          = nc "client"         (cn'    "client"             )
code            = nc "code"           (cn     "code"               )
column          = nc "column"         (cn'    "column"             ) --general enough to be in Documentation?
company         = nc "company"        (cnIES  "company"            )
component       = nc "component"      (cn'    "component"          )
concept         = nc "concept"        (cn'    "concept"            )
condition       = nc "condition"      (cn'    "condition"          )
connection      = nc "connection"     (cn'    "connection"         )
constant        = nc "constant"       (cn'    "constant"           )
constraint      = nc "constraint"     (cn'    "constraint"         )
consumer        = nc "consumer"       (cn'    "consumer"           )
content         = nc "content"        (cn'    "content"            )
context         = nc "context"        (cn'    "context"            )
coordinate      = nc "coordinate"     (cn'    "coordinate"         )
customer        = nc "customer"       (cn'    "customer"           )
datum           = nc "datum"          (cnUM   "datum"              )
decision        = nc "decision"       (cn'    "decision"           )
definition      = nc "definition"     (cn'    "definition"         )
dependency      = nc "dependency"     (cnIES  "dependency"         )
description     = nc "description"    (cn'    "description"        )
design          = nc "design"         (cn'    "design"             )
document        = nc "document"       (cn'    "document"           )
documentation   = nc "documentation"  (cn'    "documentation"      )
effect          = nc "effect"         (cn'    "effect"             )
element         = nc "element"        (cn'    "element"            )
emphasis        = nc "emphasis"       (cnIS   "emphasis"           )
endUser         = nc "end user"       (cn'    "end user"           )
environment     = nc "environment"    (cn'    "environment"        ) -- Is this term in the right spot?
example         = nc "example"        (cn'    "example"            )
failure         = nc "failure"        (cn'    "failure"            )
figure          = nc "figure"         (cn'    "figure"             )
first           = nc "first"          (cn'    "first"              ) --Does it make sense for this to be here?
form            = nc "form"           (cn'    "form"               ) 
full            = nc "full"           (cn'    "full"               ) --FIXME: Adjective
functional      = nc "functional"     (cn'    "functional"         ) --FIXME: Adjective
game            = nc "game"           (cn'    "game"               )
general         = nc "general"        (cn'    "general"            ) --FIXME: Adjective
goal            = nc "goal"           (cn'    "goal"               )
guide           = nc "guide"          (cn'    "guide"              )
implementation  = nc "implementation" (cn'    "implementation"     )
individual      = nc "individual"     (cn'    "individual"         )
information     = nc "information"    (cn     "information"        )
interest        = nc "interest"       (cn'    "interest"           )
interface       = nc "interface"      (cn'    "interface"          )
input_          = nc "input"          (cn'    "input"              )
instance_       = nc "instance"       (cn'    "instance"           )
intReader       = nc "intReader"      (cn'    "intended reader"    )
introduction    = nc "introduction"   (cn'    "introduction"       )
issue           = nc "issue"          (cn'    "issue"              )
item            = nc "item"           (cn'    "item"               )
label           = nc "label"          (cn'    "label"              )
library         = nc "library"        (cnIES  "library"            )
limitation      = nc "limitation"     (cn'    "limitation"         )
literacy        = nc "literacy"       (cnIES  "literacy"           )
loss            = nc "loss"           (cn'''  "loss"               )
material_       = nc "material"       (cn'    "material"           )
mainIdea        = nc "mainIdea"       (cn'    "main idea"          )
message         = nc "message"        (cn'    "message"            )
method_         = nc "method"         (cn'    "method"             )
module_         = nc "module"         (cn'    "module"             )
model           = nc "model"          (cn'    "model"              )
name_           = nc "name"           (cn'    "name"               )
nonfunctional   = nc "non-functional" (cn'    "non-functional"     ) --FIXME: Adjective
object          = nc "object"         (cn'    "object"             )
offShelf        = nc "Off-the-Shelf"  (cn'    "Off-the-Shelf"      )
open            = nc "open"           (cn'    "open"               )
organization    = nc "organization"   (cn'    "organization"       )
output_         = nc "output"         (cn'    "output"             )
physics         = nc "physics"        (cn'    "physics"            )
physical        = nc "physical"       (cn'    "physical"           ) --FIXME: Adjective
plan            = nc "plan"           (cn'    "plan"               )
practice        = nc "practice"       (cn'    "practice"           )
priority        = nc "priority"       (cnIES  "priority"           )
problem         = nc "problem"        (cn'    "problem"            )
procedure       = nc "procedure"      (cn'    "procedure"          )
product_        = nc "product"        (cn'    "product"            )
project         = nc "project"        (cn'    "project"            )
property        = nc "property"       (cnIES  "property"           )
purpose         = nc "purpose"        (cn'    "purpose"            )
quantity        = nc "quantity"       (cnIES  "quantity"           ) --general enough to be in documentaion.hs?
realtime        = nc "real-time"      (cn'    "real-time"          )
review          = nc "review"         (cn'    "review"             )
reference       = nc "reference"      (cn'    "reference"          )
requirement_    = nc "requirement"    (cn'    "requirement"        ) --FIXME: Eventually only have one requirement
response        = nc "response"       (cn'    "response"           )
result          = nc "result"         (cn'    "result"             )
reviewer        = nc "reviewer"       (cn'    "reviewer"           )
safety          = nc "safety"         (cnIES  "safety"             )
scope           = nc "scope"          (cn'    "scope"              )
second_         = nc "second"         (cn'    "second"             ) --Does it make sense for this to be here?
section_        = nc "section"        (cn'    "section"            )
scenario        = nc "scenario"       (cn'    "scenario"           )
source          = nc "source"         (cn'    "source"             )
simulation      = nc "simulation"     (cn'    "simulation"         )
solution        = nc "solution"       (cn'    "solution"           )
software        = nc "software"       (cn     "software"           )
summary         = nc "summary"        (cnIES  "summary"            )
specific        = nc "specific"       (cn'    "specific"           ) --FIXME: Adjective
specification   = nc "specification"  (cn'    "specification"      )
stakeholder     = nc "stakeholder"    (cn'    "stakeholder"        )
standard        = nc "standard"       (cn'    "standard"           )
statement       = nc "statement"      (cn'    "statement"          )
symbol_         = nc "symbol"         (cn'    "symbol"             )
system          = nc "system"         (cn'    "system"             )
table_          = nc "table"          (cn'    "table"              )
task            = nc "task"           (cn'    "task"               )
template        = nc "template"       (cn'    "template"           )
term_           = nc "term"           (cn'    "term"               )
terminology     = nc "terminology"    (cnIES  "terminology"        )
theory          = nc "theory"         (cnIES  "theory"             )
traceyGraph     = nc "traceyGraph"    (cn'    "traceability graph" )
traceyMatrix    = nc "traceyMatrix"   (cnICES "traceability matrix")
type_           = nc "type"           (cn'    "type"               )
uncertainty     = nc "uncertainty"    (cnIES  "uncertainty"        )
user            = nc "user"           (cn'    "user"               )
useCase         = nc "useCase"        (cn'    "use case"           )
validation      = nc "validation"     (cn'    "validation"         )
value           = nc "value"          (cn'    "value"              )
variable        = nc "variable"       (cn'    "variable"           )
verification    = nc "verification"   (cn'    "verification"       )
video           = nc "video"          (cn'    "video"              )
year            = nc "year"           (cn'    "year"               )
scpOfTheProjS   = nc "scpOfTheProj"   (cn'    "scope of the project") -- temporary generated for test


abbAcc, charOfIR, consVals, corSol, methAndAnls, orgOfDoc, procForAnls, propOfCorSol, prpsOfDoc, 
  refMat, reqInput, scpOfReq, tAuxConsts, tOfSymb, tOfUnit,
  termAndDef, traceyMandG, vav, tOfCont :: NamedChunk

abbAcc              = nc "TAbbAcc"            (abbreviation `and_PP` acronym)
consVals            = nc "consVals"           (cn "values of auxiliary constants")
corSol              = nc "corSol"             (cn' "correct solution")
charOfIR            = nc "charOfIR"           (characteristic `of_PS` intReader)
methAndAnls         = nc "methAndAnls"        (method_ `and_` analysis)
orgOfDoc            = nc "orgOfDoc"           (organization `of_` document)
procForAnls         = nc "procForAnls"        (procedure `for` analysis)
propOfCorSol        = nc "propOfCorSol"       (property `ofAPS` corSol)
prpsOfDoc           = nc "prpsOfDoc"          (purpose `of_` document)
refMat              = nc "refMat"             (cn' "reference material")
reqInput            = nc "ReqInputs"          (cn' "required input")
scpOfReq            = nc "scpOfReq"           (scope `of_` requirement)
tAuxConsts          = nc "TAuxConsts"         (cn' "auxiliary constant")
termAndDef          = nc "termAndDef"         (terminology `and_` definition)
tOfCont             = nc "tOfCont"            (table_ `of_` content)
tOfSymb             = nc "tOfSymb"            (table_ `of_` symbol_)
tOfUnit             = nc "tOfUnit"            (table_ `of_` unit_)
inDatumConstraint   = nc "InDataConstraints"  (cn' "input data constraint") -- should be moved below
outDatumConstraint  = nc "OutDataConstraints" (cn' "output data constraint")
traceyMandG         = nc "traceyMandG"        (and_TGen titleize' titleize' traceyMatrix graph)
vav                 = nc "vav"                (verification `and_` validation)

scpOfTheProj :: (NamedChunk -> Sentence) -> NamedChunk
scpOfTheProj oper = nc "scpOfTheProj" (scope `of_NINP` theGen oper project) -- reasonable hack?

-- ** Compound Chunks

designDoc, fullForm, generalSystemDescription, moduleInterface, indPRCase,
  physicalConstraint, physicalSystem, problemDescription, prodUCTable,
  specificsystemdescription, systemdescription, systemConstraint, sysCont,
  userCharacteristic, coordinateSystem, datumConstraint, inDatumConstraint, 
  outDatumConstraint, functionalRequirement, nonfunctionalRequirement, safetyReq, 
  softwareConstraint, softwareDoc, softwareReq, softwareSys, softwareVerif, 
  softwareVAV, solutionCharSpec, solutionCharacteristic, offShelfSolution, 
  physicalSim, productUC, useCaseTable, physicalProperty, vavPlan, uncertCol, userInput :: NamedChunk
 
coordinateSystem             = compoundNC coordinate system
datumConstraint              = compoundNCPP datum constraint
designDoc                    = compoundNC design document
fullForm                     = compoundNC full form
functionalRequirement        = compoundNC functional requirement_
generalSystemDescription     = compoundNC general systemdescription
moduleInterface              = compoundNC module_ interface
indPRCase                    = compoundNC individual productUC
--inDatumConstraint            = compoundNC input_ datumConstraint -- may be used later, but they break stable for now
--outDatumConstraint           = compoundNC output_ datumConstraint 
nonfunctionalRequirement     = compoundNC nonfunctional requirement_
offShelfSolution             = compoundNC offShelf solution
physicalConstraint           = compoundNC physical constraint
physicalProperty             = compoundNC physical property
physicalSim                  = compoundNC physical simulation
physicalSystem               = compoundNC physical system
problemDescription           = compoundNC problem description
prodUCTable                  = compoundNC productUC table_
productUC                    = compoundNC product_ useCase
safetyReq                    = compoundNC safety requirement_
softwareConstraint           = compoundNC software constraint
softwareDoc                  = compoundNC software documentation
softwareReq                  = compoundNCPP software requirement_
softwareSys                  = compoundNC software system
softwareVAV                  = compoundNC software vav
softwareVerif                = compoundNC software verification
solutionCharSpec             = compoundNCPSPP solutionCharacteristic specification
solutionCharacteristic       = compoundNC solution characteristic
specificsystemdescription    = compoundNC specific systemdescription
sysCont                      = compoundNC system context
systemConstraint             = compoundNC system constraint
systemdescription            = compoundNC system description
uncertCol                    = compoundNC uncertainty column
useCaseTable                 = compoundNC useCase table_
userCharacteristic           = compoundNC user characteristic
userInput                    = compoundNC user input_
vavPlan                      = compoundNC vav plan

-- * Domains

-- | Root SRS Domain.
srsDom :: ConceptChunk
srsDom = dcc "srsDom" (srs ^. term) "srs"

goalStmtDom, assumpDom, reqDom, funcReqDom, nonFuncReqDom, chgProbDom, 
  likeChgDom, unlikeChgDom, refByDom, refNameDom :: ConceptChunk
goalStmtDom   = ccs (mkIdea "goalStmtDom"   (goalStmt ^. term)                 $ Just "GS")       EmptyS [srsDom]
assumpDom     = ccs (mkIdea "assumpDom"     (assumption ^. term)               $ Just "A")        EmptyS [srsDom]
reqDom        = ccs (mkIdea "reqDom"        (requirement ^. term)              $ Just "R")        EmptyS [srsDom]
funcReqDom    = ccs (mkIdea "funcReqDom"    (functionalRequirement ^. term)    $ Just "FR")       EmptyS [reqDom]
nonFuncReqDom = ccs (mkIdea "nonFuncReqDom" (nonfunctionalRequirement ^. term) $ Just "NFR")      EmptyS [reqDom]
chgProbDom    = ccs (nc "chgProbDom" $ cn' "change")                                              EmptyS [srsDom]
likeChgDom    = ccs (mkIdea "likeChgDom"    (likelyChg ^. term)                $ Just "LC")       EmptyS [chgProbDom]
unlikeChgDom  = ccs (mkIdea "unlikeChgDom"  (unlikelyChg ^. term)              $ Just "UC")       EmptyS [chgProbDom]
refByDom      = ccs (mkIdea "refByDom"      (refBy ^. term)                    $ Just "RefBy")    EmptyS [srsDom]
refNameDom    = ccs (mkIdea "refNameDom"    (refName ^. term)                  $ Just "RefName")  EmptyS [srsDom]

-- | List of SRS-related concepts, including SRS.
srsDomains :: [ConceptChunk]
srsDomains = [cw srsDom, goalStmtDom, reqDom, funcReqDom, nonFuncReqDom, 
  assumpDom, chgProbDom, likeChgDom, unlikeChgDom, refByDom, refNameDom]

