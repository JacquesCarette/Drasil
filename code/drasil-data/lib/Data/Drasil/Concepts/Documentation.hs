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
doccon :: [IdeaDict]
doccon = [abbAcc, abbreviation, acronym, analysis, appendix, aspect, body, caseProb, charOfIR, characteristic,
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
  likelyChg, learnObj, mg, mis, notApp, physSyst, requirement, srs, thModel, typUnc, unlikelyChg, notebook]

assumption, desSpec, goalStmt, dataConst, likelyChg, learnObj, unlikelyChg, physSyst, requirement, 
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
learnObj    = commonIdeaWithDict "learnObj"    (cn' "learning objective")                            "LO"      [documentc]
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
  value, variable, video, verification, year :: IdeaDict

abbreviation    = nc "abbreviation"   (cn'    "abbreviation"       ) Nothing 
acronym         = nc "acronym"        (cn'    "acronym"            ) Nothing 
analysis        = nc "analysis"       (cnIS   "analysis"           ) Nothing 
appendix        = nc "appendix"       (cnICES "appendix"           ) Nothing 
aspect          = nc "aspect"         (cn'    "aspect"             ) Nothing 
body            = nc "body"           (cnIES  "body"               ) Nothing 
characteristic  = nc "characteristic" (cn'    "characteristic"     ) Nothing 
class_          = nc "class"          (cn'''  "class"              ) Nothing 
client          = nc "client"         (cn'    "client"             ) Nothing 
code            = nc "code"           (cn     "code"               ) Nothing 
column          = nc "column"         (cn'    "column"             ) Nothing  --general enough to be in Documentation?
company         = nc "company"        (cnIES  "company"            ) Nothing 
component       = nc "component"      (cn'    "component"          ) Nothing 
concept         = nc "concept"        (cn'    "concept"            ) Nothing 
condition       = nc "condition"      (cn'    "condition"          ) Nothing 
connection      = nc "connection"     (cn'    "connection"         ) Nothing 
constant        = nc "constant"       (cn'    "constant"           ) Nothing 
constraint      = nc "constraint"     (cn'    "constraint"         ) Nothing 
consumer        = nc "consumer"       (cn'    "consumer"           ) Nothing 
content         = nc "content"        (cn'    "content"            ) Nothing 
context         = nc "context"        (cn'    "context"            ) Nothing 
coordinate      = nc "coordinate"     (cn'    "coordinate"         ) Nothing 
customer        = nc "customer"       (cn'    "customer"           ) Nothing 
datum           = nc "datum"          (cnUM   "datum"              ) Nothing 
decision        = nc "decision"       (cn'    "decision"           ) Nothing 
definition      = nc "definition"     (cn'    "definition"         ) Nothing 
dependency      = nc "dependency"     (cnIES  "dependency"         ) Nothing 
description     = nc "description"    (cn'    "description"        ) Nothing 
design          = nc "design"         (cn'    "design"             ) Nothing 
document        = nc "document"       (cn'    "document"           ) Nothing 
documentation   = nc "documentation"  (cn'    "documentation"      ) Nothing 
effect          = nc "effect"         (cn'    "effect"             ) Nothing 
element         = nc "element"        (cn'    "element"            ) Nothing 
emphasis        = nc "emphasis"       (cnIS   "emphasis"           ) Nothing 
endUser         = nc "end user"       (cn'    "end user"           ) Nothing 
environment     = nc "environment"    (cn'    "environment"        ) Nothing  -- Is this term in the right spot?
example         = nc "example"        (cn'    "example"            ) Nothing 
failure         = nc "failure"        (cn'    "failure"            ) Nothing 
figure          = nc "figure"         (cn'    "figure"             ) Nothing 
first           = nc "first"          (cn'    "first"              ) Nothing  --Does it make sense for this to be here?
form            = nc "form"           (cn'    "form"               ) Nothing  
full            = nc "full"           (cn'    "full"               ) Nothing  --FIXME: Adjective
functional      = nc "functional"     (cn'    "functional"         ) Nothing  --FIXME: Adjective
game            = nc "game"           (cn'    "game"               ) Nothing 
general         = nc "general"        (cn'    "general"            ) Nothing  --FIXME: Adjective
goal            = nc "goal"           (cn'    "goal"               ) Nothing 
guide           = nc "guide"          (cn'    "guide"              ) Nothing 
implementation  = nc "implementation" (cn'    "implementation"     ) Nothing 
individual      = nc "individual"     (cn'    "individual"         ) Nothing 
information     = nc "information"    (cn     "information"        ) Nothing 
interest        = nc "interest"       (cn'    "interest"           ) Nothing 
interface       = nc "interface"      (cn'    "interface"          ) Nothing 
input_          = nc "input"          (cn'    "input"              ) Nothing 
instance_       = nc "instance"       (cn'    "instance"           ) Nothing 
intReader       = nc "intReader"      (cn'    "intended reader"    ) Nothing 
introduction    = nc "introduction"   (cn'    "introduction"       ) Nothing 
issue           = nc "issue"          (cn'    "issue"              ) Nothing 
item            = nc "item"           (cn'    "item"               ) Nothing 
label           = nc "label"          (cn'    "label"              ) Nothing 
library         = nc "library"        (cnIES  "library"            ) Nothing 
limitation      = nc "limitation"     (cn'    "limitation"         ) Nothing 
literacy        = nc "literacy"       (cnIES  "literacy"           ) Nothing 
loss            = nc "loss"           (cn'''  "loss"               ) Nothing 
material_       = nc "material"       (cn'    "material"           ) Nothing 
mainIdea        = nc "mainIdea"       (cn'    "main idea"          ) Nothing 
message         = nc "message"        (cn'    "message"            ) Nothing 
method_         = nc "method"         (cn'    "method"             ) Nothing 
module_         = nc "module"         (cn'    "module"             ) Nothing 
model           = nc "model"          (cn'    "model"              ) Nothing 
name_           = nc "name"           (cn'    "name"               ) Nothing 
nonfunctional   = nc "non-functional" (cn'    "non-functional"     ) Nothing  --FIXME: Adjective
object          = nc "object"         (cn'    "object"             ) Nothing 
offShelf        = nc "Off-the-Shelf"  (cn'    "Off-the-Shelf"      ) Nothing 
open            = nc "open"           (cn'    "open"               ) Nothing 
organization    = nc "organization"   (cn'    "organization"       ) Nothing 
output_         = nc "output"         (cn'    "output"             ) Nothing 
physics         = nc "physics"        (cn'    "physics"            ) Nothing 
physical        = nc "physical"       (cn'    "physical"           ) Nothing  --FIXME: Adjective
plan            = nc "plan"           (cn'    "plan"               ) Nothing 
practice        = nc "practice"       (cn'    "practice"           ) Nothing 
priority        = nc "priority"       (cnIES  "priority"           ) Nothing 
problem         = nc "problem"        (cn'    "problem"            ) Nothing 
procedure       = nc "procedure"      (cn'    "procedure"          ) Nothing 
product_        = nc "product"        (cn'    "product"            ) Nothing 
project         = nc "project"        (cn'    "project"            ) Nothing 
property        = nc "property"       (cnIES  "property"           ) Nothing 
purpose         = nc "purpose"        (cn'    "purpose"            ) Nothing 
quantity        = nc "quantity"       (cnIES  "quantity"           ) Nothing  --general enough to be in documentaion.hs?
realtime        = nc "real-time"      (cn'    "real-time"          ) Nothing 
review          = nc "review"         (cn'    "review"             ) Nothing 
reference       = nc "reference"      (cn'    "reference"          ) Nothing 
requirement_    = nc "requirement"    (cn'    "requirement"        ) Nothing  --FIXME: Eventually only have one requirement
response        = nc "response"       (cn'    "response"           ) Nothing 
result          = nc "result"         (cn'    "result"             ) Nothing 
reviewer        = nc "reviewer"       (cn'    "reviewer"           ) Nothing 
safety          = nc "safety"         (cnIES  "safety"             ) Nothing 
scope           = nc "scope"          (cn'    "scope"              ) Nothing 
second_         = nc "second"         (cn'    "second"             ) Nothing  --Does it make sense for this to be here?
section_        = nc "section"        (cn'    "section"            ) Nothing 
scenario        = nc "scenario"       (cn'    "scenario"           ) Nothing 
source          = nc "source"         (cn'    "source"             ) Nothing 
simulation      = nc "simulation"     (cn'    "simulation"         ) Nothing 
solution        = nc "solution"       (cn'    "solution"           ) Nothing 
software        = nc "software"       (cn     "software"           ) Nothing 
summary         = nc "summary"        (cnIES  "summary"            ) Nothing 
specific        = nc "specific"       (cn'    "specific"           ) Nothing  --FIXME: Adjective
specification   = nc "specification"  (cn'    "specification"      ) Nothing 
stakeholder     = nc "stakeholder"    (cn'    "stakeholder"        ) Nothing 
standard        = nc "standard"       (cn'    "standard"           ) Nothing 
statement       = nc "statement"      (cn'    "statement"          ) Nothing 
symbol_         = nc "symbol"         (cn'    "symbol"             ) Nothing 
system          = nc "system"         (cn'    "system"             ) Nothing 
table_          = nc "table"          (cn'    "table"              ) Nothing 
task            = nc "task"           (cn'    "task"               ) Nothing 
template        = nc "template"       (cn'    "template"           ) Nothing 
term_           = nc "term"           (cn'    "term"               ) Nothing 
terminology     = nc "terminology"    (cnIES  "terminology"        ) Nothing 
theory          = nc "theory"         (cnIES  "theory"             ) Nothing 
traceyGraph     = nc "traceyGraph"    (cn'    "traceability graph" ) Nothing 
traceyMatrix    = nc "traceyMatrix"   (cnICES "traceability matrix") Nothing 
type_           = nc "type"           (cn'    "type"               ) Nothing 
uncertainty     = nc "uncertainty"    (cnIES  "uncertainty"        ) Nothing 
user            = nc "user"           (cn'    "user"               ) Nothing 
useCase         = nc "useCase"        (cn'    "use case"           ) Nothing 
validation      = nc "validation"     (cn'    "validation"         ) Nothing 
value           = nc "value"          (cn'    "value"              ) Nothing 
variable        = nc "variable"       (cn'    "variable"           ) Nothing 
verification    = nc "verification"   (cn'    "verification"       ) Nothing 
video           = nc "video"          (cn'    "video"              ) Nothing 
year            = nc "year"           (cn'    "year"               ) Nothing 
scpOfTheProjS   = nc "scpOfTheProj"   (cn'    "scope of the project") Nothing -- temporary generated for test


abbAcc, caseProb, charOfIR, consVals, corSol, methAndAnls, orgOfDoc, procForAnls, propOfCorSol, prpsOfDoc, 
  refMat, reqInput, scpOfReq, tAuxConsts, tOfSymb, tOfUnit,
  termAndDef, traceyMandG, vav, tOfCont :: IdeaDict

abbAcc              = nc "TAbbAcc"            (abbreviation `and_PP` acronym)                   Nothing
caseProb            = nc "caseProb"           (cn' "case problem")                              Nothing
consVals            = nc "consVals"           (cn "values of auxiliary constants")              Nothing
corSol              = nc "corSol"             (cn' "correct solution")                          Nothing
charOfIR            = nc "charOfIR"           (characteristic `of_PS` intReader)                Nothing
methAndAnls         = nc "methAndAnls"        (method_ `and_` analysis)                         Nothing
orgOfDoc            = nc "orgOfDoc"           (organization `of_` document)                     Nothing
procForAnls         = nc "procForAnls"        (procedure `for` analysis)                        Nothing
propOfCorSol        = nc "propOfCorSol"       (property `ofAPS` corSol)                         Nothing
prpsOfDoc           = nc "prpsOfDoc"          (purpose `of_` document)                          Nothing
refMat              = nc "refMat"             (cn' "reference material")                        Nothing
reqInput            = nc "ReqInputs"          (cn' "required input")                            Nothing
scpOfReq            = nc "scpOfReq"           (scope `of_` requirement)                         Nothing
tAuxConsts          = nc "TAuxConsts"         (cn' "auxiliary constant")                        Nothing
termAndDef          = nc "termAndDef"         (terminology `and_` definition)                   Nothing
tOfCont             = nc "tOfCont"            (table_ `of_` content)                            Nothing
tOfSymb             = nc "tOfSymb"            (table_ `of_` symbol_)                            Nothing
tOfUnit             = nc "tOfUnit"            (table_ `of_` unit_)                              Nothing
inDatumConstraint   = nc "InDataConstraints"  (cn' "input data constraint")                     Nothing -- should be moved below 
outDatumConstraint  = nc "OutDataConstraints" (cn' "output data constraint")                    Nothing
traceyMandG         = nc "traceyMandG"        (and_TGen titleize' titleize' traceyMatrix graph) Nothing
vav                 = nc "vav"                (verification `and_` validation)                  Nothing

scpOfTheProj :: (IdeaDict -> Sentence) -> IdeaDict
scpOfTheProj oper = nc "scpOfTheProj" (scope `of_NINP` theGen oper project) Nothing-- reasonable hack?

-- ** Compound Chunks

designDoc, fullForm, generalSystemDescription, moduleInterface, indPRCase,
  physicalConstraint, physicalSystem, problemDescription, prodUCTable,
  specificsystemdescription, systemdescription, systemConstraint, sysCont,
  userCharacteristic, coordinateSystem, datumConstraint, inDatumConstraint, 
  outDatumConstraint, functionalRequirement, nonfunctionalRequirement, safetyReq, 
  softwareConstraint, softwareDoc, softwareReq, softwareSys, softwareVerif, 
  softwareVAV, solutionCharSpec, solutionCharacteristic, offShelfSolution, 
  physicalSim, productUC, useCaseTable, physicalProperty, vavPlan, uncertCol, userInput :: IdeaDict
 
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
chgProbDom    = ccs (nc "chgProbDom" (cn' "change") Nothing)                                      EmptyS [srsDom]
likeChgDom    = ccs (mkIdea "likeChgDom"    (likelyChg ^. term)                $ Just "LC")       EmptyS [chgProbDom]
unlikeChgDom  = ccs (mkIdea "unlikeChgDom"  (unlikelyChg ^. term)              $ Just "UC")       EmptyS [chgProbDom]
refByDom      = ccs (mkIdea "refByDom"      (refBy ^. term)                    $ Just "RefBy")    EmptyS [srsDom]
refNameDom    = ccs (mkIdea "refNameDom"    (refName ^. term)                  $ Just "RefName")  EmptyS [srsDom]

-- | List of SRS-related concepts, including SRS.
srsDomains :: [ConceptChunk]
srsDomains = [cw srsDom, goalStmtDom, reqDom, funcReqDom, nonFuncReqDom, 
  assumpDom, chgProbDom, likeChgDom, unlikeChgDom, refByDom, refNameDom]

