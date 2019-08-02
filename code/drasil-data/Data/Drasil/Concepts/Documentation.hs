module Data.Drasil.Concepts.Documentation where

import Language.Drasil hiding (organization, year)
import Utils.Drasil

import Data.Drasil.Concepts.Math (graph, unit_)
import Data.Drasil.IdeaDicts (dataDefn, documentc, genDefn, inModel, softEng, thModel)

import Control.Lens ((^.))

doccon :: [NamedChunk]
doccon = [abbreviation, analysis, appendix, aspect, body, charOfIR, characteristic,
  class_, client, code, column, company, component, concept, condition, connection,
  consVals, constant, constraint, consumer, content, context, coordinate, corSol,
  customer, datum, datumConstraint, decision, definition, dependency, description,
  design, designDoc, document, documentation, effect, element, emphasis, endUser,
  environment, failure, figure, first, form, full, fullForm, functional,
  functionalRequirement, game, general, generalSystemDescription, goal, guide,
  implementation, indPRCase, individual, information, input_, instance_, intReader,
  interest, interface, introduction, issue, item, label, library, limitation,
  literacy, loss, material_, message, method_, model, module_, name_, nonfunctional,
  nonfunctionalRequirement, object, offShelf, offShelfSolution, open, orgOfDoc,
  organization, output_, physical, physicalConstraint, physicalProperty, physicalSim,
  physicalSystem, physics, plan, practice, priority, problem, problemDescription,
  prodUCTable, productUC, product_, project, propOfCorSol, property, prpsOfDoc,
  purpose, quantity, realtime, reference, refmat, requirement_, response, result,
  reviewer, safety, safetyReq, scenario, scope, scpOfReq, scpOfTheProjS, second_,
  section_, simulation, software, softwareConstraint, softwareDoc, softwareReq,
  softwareSys, softwareVAV, softwareVerif, solution, solutionCharSpec,
  solutionCharacteristic, source, specific, specification, specificsystemdescription,
  stakeholder, standard, statement, symbol_, sysCont, system, systemConstraint,
  systemdescription, tOfSymb, tOfUnit, table_, task, template, termAndDef, term_,
  terminology, theory, traceyGraph, traceyMandG, traceyMatrix, type_, uncertCol,
  uncertainty, useCase, useCaseTable, user, userCharacteristic, userInput,
  validation, value, variable, vav, vavPlan, verification, video, year]

doccon' :: [CI]
doccon' = [assumption, dataDefn, desSpec, genDefn, goalStmt, dataConst, inModel, likelyChg,
  unlikelyChg, physSyst, requirement, thModel, mg, mis, notApp, srs, typUnc]

assumption, desSpec, goalStmt, dataConst, likelyChg, unlikelyChg, physSyst, requirement,
  mg, mis, notApp, srs, typUnc, sec :: CI

-----------------------------------------------------------------------------------------------------------------
-- | CI       |           |    uid      |         term                        | abbreviation |     ConceptDomain
-----------------------------------------------------------------------------------------------------------------
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                  "A"         [softEng]
desSpec     = commonIdeaWithDict "desSpec"     (fterms compoundPhrase design specification)        "DS"        [softEng]
goalStmt    = commonIdeaWithDict "goalStmt"    (fterms compoundPhrase goal statement)              "GS"        [softEng]
dataConst   = commonIdeaWithDict "dataConst"   (cn' "data constraint")                             "DC"        [softEng]
likelyChg   = commonIdeaWithDict "likelyChg"   (cn' "likely change")                               "LC"        [softEng]
unlikelyChg = commonIdeaWithDict "unlikelyChg" (cn' "unlikely change")                             "UC"        [softEng]
physSyst    = commonIdeaWithDict "physSyst"    (fterms compoundPhrase physicalSystem description)  "PS"        [softEng]
requirement = commonIdeaWithDict "requirement" (cn' "requirement")                                 "R"         [softEng]
mis         = commonIdeaWithDict "mis"         (fterms compoundPhrase moduleInterface specification) "MIS"        [softEng]
mg          = commonIdeaWithDict "mg"          (fterms compoundPhrase module_ guide)               "MG"        [softEng]
notApp      = commonIdea         "notApp"      (nounPhraseSP "not applicable")                     "N/A"       []
typUnc      = commonIdeaWithDict "typUnc"      (cn' "typical uncertainty")                         "Uncert."   [softEng]
sec         = commonIdeaWithDict "section"     (cn' "section")                                     "Sec"       [documentc]
srs = commonIdeaWithDict "srs" 
  (compoundPhraseP1 (softwareReq ^. term) (specification ^. term))
  "SRS" [softEng]

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

abbreviation, analysis, appendix, aspect, body, characteristic, class_, client, 
  code, column, company, component, concept, condition, connection, constant,
  constraint, consumer, content, context, coordinate, customer, datum, decision, 
  definition, dependency, description, design, document, documentation, effect, 
  element, emphasis, endUser, environment, failure, figure, first, form, full, 
  functional, game, general, goal, guide, implementation, individual, information, 
  interest, interface, input_, instance_, intReader, introduction, issue, item, 
  loss, label, library, limitation, literacy, material_, message, method_, module_,
  model, name_, nonfunctional, object, offShelf, open, organization, output_,
  physics, physical, plan, practice, priority, problem, product_, project,
  property, purpose, quantity, realtime, reference, requirement_, response, 
  result, reviewer, safety, scope, second_, section_, scenario, source,
  simulation, software, solution, specific, specification, stakeholder,
  standard, statement, symbol_, system, table_, task, template, term_,
  terminology, theory, traceyGraph, traceyMatrix, type_, uncertainty, user,
  useCase, validation, value, variable, video, verification, year,
  scpOfTheProjS :: NamedChunk

abbreviation    = nc "abbreviation"   (cn'    "abbreviation"       )
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
product_        = nc "product"        (cn'    "product"            )
project         = nc "project"        (cn'    "project"            )
property        = nc "property"       (cnIES  "property"           )
purpose         = nc "purpose"        (cn'    "purpose"            )
quantity        = nc "quantity"       (cnIES  "quantity"           ) --general enough to be in documentaion.hs?
realtime        = nc "real-time"      (cn'    "real-time"          )
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


charOfIR, consVals, corSol, orgOfDoc, propOfCorSol, prpsOfDoc, refmat,
  scpOfReq, tOfSymb, tOfUnit, termAndDef, traceyMandG, vav :: NamedChunk

consVals     = nc "consVals"     (cn "values of auxiliary constants")
corSol       = nc "corSol"       (cn' "correct solution")
charOfIR     = nc "charOfIR"     (characteristic `of__` intReader)
orgOfDoc     = nc "orgOfDoc"     (organization `of_` document)
propOfCorSol = nc "propOfCorSol" (property `ofA` corSol)
prpsOfDoc    = nc "prpsOfDoc"    (purpose `of_` document)
refmat       = nc "refmat"       (cn' "reference material")
scpOfReq     = nc "scpOfReq"     (scope `of_'` requirement)
termAndDef   = nc "termAndDef"   (terminology `and_'` definition)
tOfSymb      = nc "tOfSymb"      (table_ `of_'` symbol_)
tOfUnit      = nc "tOfUnit"      (table_ `of_'` unit_)
traceyMandG  = nc "traceyMandG"  (andRT titleize' titleize' traceyMatrix graph)
vav          = nc "vav"          (verification `and_` validation)

scpOfTheProj :: (NamedChunk -> Sentence) -> NamedChunk
scpOfTheProj oper = nc "scpOfTheProj" (scope `ofN_` theCustom oper project) -- reasonable hack?

-- compounds

designDoc, fullForm, generalSystemDescription, moduleInterface, indPRCase,
  physicalConstraint, physicalSystem, problemDescription, prodUCTable,
  specificsystemdescription, systemdescription, systemConstraint, sysCont,
  userCharacteristic, datumConstraint, functionalRequirement,
  nonfunctionalRequirement, safetyReq, softwareConstraint, softwareDoc,
  softwareReq, softwareSys, softwareVerif, softwareVAV, solutionCharSpec,
  solutionCharacteristic, offShelfSolution, physicalSim, productUC, 
  useCaseTable, physicalProperty, vavPlan, uncertCol, userInput :: NamedChunk
 
datumConstraint              = compoundNC' datum constraint
designDoc                    = compoundNC design document
fullForm                     = compoundNC full form
functionalRequirement        = compoundNC functional requirement_
generalSystemDescription     = compoundNC general systemdescription
moduleInterface              = compoundNC module_ interface
indPRCase                    = compoundNC individual productUC
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
softwareReq                  = compoundNC' software requirement_
softwareSys                  = compoundNC software system
softwareVAV                  = compoundNC software vav
softwareVerif                = compoundNC software verification
solutionCharSpec             = compoundNCP1 solutionCharacteristic specification
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

-- Domains

--Root SRS Domain
srsDom :: CommonConcept
srsDom = dcc' "srsDom" (srs ^. term) "srs" ""

goalStmtDom :: ConceptChunk
goalStmtDom = ccs (mkIdea "goalStmtDom" (goalStmt ^. term) $ Just "GS") EmptyS
  [srsDom]

assumpDom :: ConceptChunk
assumpDom = ccs (mkIdea "assumpDom" (assumption ^. term) $ Just "A") EmptyS [srsDom]

reqDom :: ConceptChunk
reqDom = ccs (mkIdea "reqDom" (requirement ^. term) $ Just "R") EmptyS [srsDom]

funcReqDom :: ConceptChunk
funcReqDom = ccs (mkIdea "funcReqDom" (functionalRequirement ^. term) $ Just "FR") EmptyS [reqDom]

nonFuncReqDom :: ConceptChunk
nonFuncReqDom = ccs (mkIdea "nonFuncReqDom" (nonfunctionalRequirement ^. term) $
  Just "NFR") EmptyS [reqDom]

chgProbDom :: ConceptChunk
chgProbDom = ccs (nc "chgProbDom" $ cn' "change") EmptyS [srsDom]

likeChgDom :: ConceptChunk
likeChgDom = ccs (mkIdea "likeChgDom" (likelyChg ^. term) $ Just "LC") EmptyS [chgProbDom]

unlikeChgDom :: ConceptChunk
unlikeChgDom = ccs (mkIdea "unlikeChgDom" (unlikelyChg ^. term) $ Just "UC") EmptyS [chgProbDom]
-- | List of domains for SRS
srsDomains :: [ConceptChunk]
srsDomains = [cw srsDom, goalStmtDom, reqDom, funcReqDom, nonFuncReqDom, assumpDom,
  chgProbDom, likeChgDom, unlikeChgDom]

-- FIXME: fterms is here instead of Utils because of cyclic import
-- | Apply a binary function to the terms of two named ideas, instead of to the named
-- ideas themselves. Ex. @fterms compoundPhrase t1 t2@ instead of
-- @compoundPhrase (t1 ^. term) (t2 ^. term)@
fterms :: (NamedIdea c, NamedIdea d) => (NP -> NP -> t) -> c -> d -> t
fterms f a b = f (a ^. term) (b ^. term)
