module Data.Drasil.Concepts.Documentation where

import Prelude hiding (id)
import Language.Drasil

import Data.Drasil.Concepts.Math (graph)
import Control.Lens ((^.))
import qualified Language.Drasil.NounPhrase as NP

-- acronyms to be used throughout
-- ex. S "as seen in (A1)" -> S "as seen in" +:+ sParen (acroA "1")
acroA, acroDD, acroGD, acroGS, acroIM, acroLC, acroPS
  , acroR, acroT :: Int -> Sentence

acroA  numVar = short assumption  :+: S (show numVar)
acroDD numVar = short dataDefn    :+: S (show numVar)
acroGD numVar = short genDefn     :+: S (show numVar)
acroGS numVar = short goalStmt    :+: S (show numVar)
acroIM numVar = short inModel     :+: S (show numVar)
acroLC numVar = short likelyChg   :+: S (show numVar)
acroPS numVar = short physSyst    :+: S (show numVar)
acroR  numVar = short requirement :+: S (show numVar)
acroT  numVar = short thModel     :+: S (show numVar)


-- Creates an ordered list of items to be referenced.
-- Helps with the formatting of HTML documents for the most part.
-- Takes a list of Contents (e.g. Assumptions, LikelyChanges) and a
-- number from which to start counting.
acroNumGen :: [Contents] -> Int -> [Contents]
acroNumGen [] _ = []
acroNumGen (first:rest) num = (f first) : acroNumGen rest (num + 1)
  where f (Assumption a) = Assumption $ nw $ npnc' (a ^. id) (a ^. term) ("A" ++  (show num))
        f (Requirement r) = Requirement $ ReqChunk (nw $ npnc' (r ^. id) (r ^. term) ("R" ++ (show num))) []
        f (LikelyChange lc) = LikelyChange $ LCChunk (nw $ npnc' (lc ^. id) (lc ^. term) ("LC" ++ (show num))) []

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, unlikelyChg,
  physSyst, requirement, srs, thModel, mg, desSpec, notApp, dataConst :: CI

--------------------------------------------------------------------------------
-- CI       |           |    id       |         term            | abbreviation |
--------------------------------------------------------------------------------
assumption  = commonIdea "assumption"  (cn' "assumption")                                  "A"
dataDefn    = commonIdea "dataDefn"    (cn' "data definition")                             "DD"
desSpec     = commonIdea "desSpec"     (fterms compoundPhrase design specification)        "DS"
genDefn     = commonIdea "genDefn"     (cn' "general definition")                          "GD"
goalStmt    = commonIdea "goalStmt"    (fterms compoundPhrase goal statement)              "GS"
dataConst   = commonIdea "dataConst"   (cn' "data constraint")                             "DC"
inModel     = commonIdea "inModel"     (fterms compoundPhrase instance_ model)             "IM"
likelyChg   = commonIdea "likelyChg"   (cn' "likely change")                               "LC"
unlikelyChg = commonIdea "unlikelyChg" (cn' "unlikely change")                             "UC"
physSyst    = commonIdea "physSyst"    (fterms compoundPhrase physicalSystem description)  "PS"
requirement = commonIdea "requirement" (cn' "requirement")                                 "R"
thModel     = commonIdea "thModel"     (cn' "theoretical model")                           "T"
mg          = commonIdea "mg"          (fterms compoundPhrase module_ guide)               "MG"
notApp      = commonIdea "notApp"      (nounPhraseSP "not applicable")                     "N/A"

srs = commonIdea "srs" 
  (compoundPhrase''' NP.plural (softwareReq ^. term) (specification ^. term))
  "SRS"

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

analysis, appendix, body, characteristic, class_, client, code, column, company, 
  component, concept, condition, constraint, consumer, connection, content, context,
  coordinate, customer, datum, decision, definition, dependency, description,
  design, document, documentation, effect, element, endUser, environment, failure,
  figure, first, functional, game, general, goal, guide, implementation, individual, 
  information, interest, interface, input_, instance_, intReader, introduction, 
  issue, item, loss, label, library, limitation, literacy, material_, message, 
  method_, module_, model, name_, nonfunctional, object, offShelf, open, organization,
  output_, physics, physical, plan, practice, priority, problem, product_, project, 
  property, purpose, quantity, realtime, reference, requirement_, response, reviewer, 
  safety, scope, second_, section_, scenario, source, simulation, software, solution,
  specific, specification, stakeholder, standard, statement, symbol_, system, table_,
  task, template, term_, terminology, theory, traceyGraph, traceyMatrix, uncertainty,
  user, useCase, validation, value, variable, video, verification, year :: NamedChunk

analysis        = npnc "analysis"       (cnIS   "analysis"           )
appendix        = npnc "appendix"       (cnICES "appendix"           )
body            = npnc "body"           (cnIES  "body"               )
characteristic  = npnc "characteristic" (cn'    "characteristic"     )
class_          = npnc "class"          (cn'''  "class"              )
client          = npnc "client"         (cn'    "client"             )
code            = npnc "code"           (cn     "code"               )
column          = npnc "column"         (cn'    "column"             ) --general enough to be in Documentation?
company         = npnc "company"        (cnIES  "company"            )
component       = npnc "component"      (cn'    "component"          )
concept         = npnc "concept"        (cn'    "concept"            )
condition       = npnc "condition"      (cn'    "condition"          )
connection      = npnc "connection"     (cn'    "connection"         )
constraint      = npnc "constraint"     (cn'    "constraint"         )
consumer        = npnc "consumer"       (cn'    "consumer"           )
content         = npnc "content"        (cn'    "content"            )
context         = npnc "context"        (cn'    "context"            )
coordinate      = npnc "coordinate"     (cn'    "coordinate"         )
customer        = npnc "customer"       (cn'    "customer"           )
datum           = npnc "datum"          (cnUM   "datum"              )
decision        = npnc "decision"       (cn'    "decision"           )
definition      = npnc "definition"     (cn'    "definition"         )
dependency      = npnc "dependency"     (cnIES  "dependency"         )
description     = npnc "description"    (cn'    "description"        )
design          = npnc "design"         (cn'    "design"             )
document        = npnc "document"       (cn'    "document"           )
documentation   = npnc "documentation"  (cn'    "documentation"      )
effect          = npnc "effect"         (cn'    "effect"             )
element         = npnc "element"        (cn'    "element"            )
endUser         = npnc "end user"       (cn'    "end user"           )
environment     = npnc "environment"    (cn'    "environment"        ) -- Is this term in the right spot?
failure         = npnc "failure"        (cn'    "failure"            )
figure          = npnc "figure"         (cn'    "figure"             )
first           = npnc "first"          (cn'    "first"              ) --Does it make sense for this to be here?
functional      = npnc "functional"     (cn'    "functional"         ) --FIXME: Adjective
game            = npnc "game"           (cn'    "game"               )
general         = npnc "general"        (cn'    "general"            ) --FIXME: Adjective
goal            = npnc "goal"           (cn'    "goal"               )
guide           = npnc "guide"          (cn'    "guide"              )
implementation  = npnc "implementation" (cn'    "implementation"     )
individual      = npnc "individual"     (cn'    "individual"         )
information     = npnc "information"    (cn     "information"        )
interest        = npnc "interest"       (cn'    "interest"           )
interface       = npnc "interface"      (cn'    "interface"          )
input_          = npnc "input"          (cn'    "input"              )
instance_       = npnc "instance"       (cn'    "instance"           )
intReader       = npnc "intReader"      (cn'    "intended reader"    )
introduction    = npnc "introduction"   (cn'    "introduction"       )
issue           = npnc "issue"          (cn'    "issue"              )
item            = npnc "item"           (cn'    "item"               )
label           = npnc "label"          (cn'    "label"              )
library         = npnc "library"        (cnIES  "library"            )
limitation      = npnc "limitation"     (cn'    "limitation"         )
literacy        = npnc "literacy"       (cnIES  "literacy"           )
loss            = npnc "loss"           (cn'''  "loss"               )
material_       = npnc "material"       (cn'    "material"           )
message         = npnc "message"        (cn'    "message"            )
method_         = npnc "method"         (cn'    "method"             )
module_         = npnc "module"         (cn'    "module"             )
model           = npnc "model"          (cn'    "model"              )
name_           = npnc "name"           (cn'    "name"               )
nonfunctional   = npnc "non-functional" (cn'    "non-functional"     ) --FIXME: Adjective
object          = npnc "object"         (cn'    "object"             )
offShelf        = npnc "Off-the-Shelf"  (cn'    "Off-the-Shelf"      )
open            = npnc "open"           (cn'    "open"               )
organization    = npnc "organization"   (cn'    "organization"       )
output_         = npnc "output"         (cn'    "output"             )
physics         = npnc "physics"        (cn'    "physics"            )
physical        = npnc "physical"       (cn'    "physical"           ) --FIXME: Adjective
plan            = npnc "plan"           (cn'    "plan"               )
practice        = npnc "practice"       (cn'    "practice"           )
priority        = npnc "priority"       (cnIES  "priority"           )
problem         = npnc "problem"        (cn'    "problem"            )
product_        = npnc "product"        (cn'    "product"            )
project         = npnc "project"        (cn'    "project"            )
property        = npnc "property"       (cnIES  "property"           )
purpose         = npnc "purpose"        (cn'    "purpose"            )
quantity        = npnc "quantity"       (cnIES  "quantity"           ) --general enough to be in documentaion.hs?
realtime        = npnc "real-time"      (cn'    "real-time"          )
reference       = npnc "reference"      (cn'    "reference"          )
requirement_    = npnc "requirement"    (cn'    "requirement"        ) --FIXME: Eventually only have one requirement
response        = npnc "response"       (cn'    "response"           )
reviewer        = npnc "reviewer"       (cn'    "reviewer"           )
safety          = npnc "safety"         (cnIES  "safety"             )
scope           = npnc "scope"          (cn'    "scope"              )
second_         = npnc "second"         (cn'    "second"             ) --Does it make sense for this to be here?
section_        = npnc "section"        (cn'    "section"            )
scenario        = npnc "scenario"       (cn'    "scenario"           )
source          = npnc "source"         (cn'    "source"             )
simulation      = npnc "simulation"     (cn'    "simulation"         )
solution        = npnc "solution"       (cn'    "solution"           )
software        = npnc "software"       (cn     "software"           )
specific        = npnc "specific"       (cn'    "specific"           ) --FIXME: Adjective
specification   = npnc "specification"  (cn'    "specification"      )
stakeholder     = npnc "stakeholder"    (cn'    "stakeholder"        )
standard        = npnc "standard"       (cn'    "standard"           )
statement       = npnc "statement"      (cn'    "statement"          )
symbol_         = npnc "symbol"         (cn'    "symbol"             )
system          = npnc "system"         (cn'    "system"             )
table_          = npnc "table"          (cn'    "table"              )
task            = npnc "task"           (cn'    "task"               )
template        = npnc "template"       (cn'    "template"           )
term_           = npnc "term"           (cn'    "term"               )
terminology     = npnc "terminology"    (cnIES  "terminology"        )
theory          = npnc "theory"         (cnIES  "theory"             )
traceyGraph     = npnc "traceyGraph"    (cn'    "traceability graph" )
traceyMatrix    = npnc "traceyMatrix"   (cnICES "traceability matrix")
uncertainty     = npnc "uncertainty"    (cnIES  "uncertainty"        )
user            = npnc "user"           (cn'    "user"               )
useCase         = npnc "useCase"        (cn'    "use case"           )
validation      = npnc "validation"     (cn'    "validation"         )
value           = npnc "value"          (cn'    "value"              )
variable        = npnc "variable"       (cn'    "variable"           )
verification    = npnc "verification"   (cn'    "verification"       )
video           = npnc "video"          (cn'    "video"              )
year            = npnc "year"           (cn'    "year"               )


orgOfDoc, prpsOfDoc, refmat, scpOfReq, consVals,
  termAndDef, tOfSymb, traceyMandG, corSol, charOfIR, propOfCorSol, vav :: NamedChunk

corSol       = npnc "corSol"       (cn' "correct solution")
charOfIR     = npnc "charOfIR"     (characteristic `of__` intReader)
orgOfDoc     = npnc "orgOfDoc"     (organization `of_` document)
propOfCorSol = npnc "propOfCorSol" (property `ofA` corSol)
prpsOfDoc    = npnc "prpsOfDoc"    (purpose `of_` document)
refmat       = npnc "refmat"       (cn' "reference material")
scpOfReq     = npnc "scpOfReq"     (scope `of_'` requirement)
termAndDef   = npnc "termAndDef"   (terminology `and_'` definition)
tOfSymb      = npnc "tOfSymb"      (table_ `of_'` symbol_)
traceyMandG  = npnc "traceyMandG"  (andRT titleize' titleize' traceyMatrix graph)
vav          = npnc "vav"          (verification `and_` validation)
consVals     = npnc "consVals"     (cn "values of auxiliary constants")

scpOfTheProj :: (NamedChunk -> Sentence) -> NamedChunk
scpOfTheProj oper = npnc "scpOfTheProj" (scope `of_` theCustom oper project) -- reasonable hack?

-- compounds

designDoc, generalSystemDescription, indPRCase,
  physicalConstraint, physicalSystem, problemDescription, prodUCTable,
  specificsystemdescription, systemdescription, systemConstraint, sysCont,
  userCharacteristic, datumConstraint, functionalRequirement,
  nonfunctionalRequirement, safetyReq, softwareConstraint, softwareDoc,
  softwareReq, softwareSys, softwareVerif, softwareVAV, solutionCharSpec,
  solutionCharacteristic, offShelfSolution, physicalSim, productUC, 
  useCaseTable, physicalProperty, vavPlan, uncertCol, userInput :: NamedChunk
 
datumConstraint              = compoundNC' datum constraint
designDoc                    = compoundNC design document
functionalRequirement        = compoundNC functional requirement_
generalSystemDescription     = compoundNC general systemdescription
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
solutionCharSpec             = compoundNC''' (NP.plural) solutionCharacteristic specification
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

-- extra utilities --
missing :: Sentence
missing = S "..."

-- FIXME: fterms is here instead of Utils because of cyclic import
-- | Apply a binary function to the terms of two named ideas, instead of to the named
-- ideas themselves. Ex. @fterms compoundPhrase t1 t2@ instead of
-- @compoundPhrase (t1 ^. term) (t2 ^. term)@
fterms :: (NamedIdea c, NamedIdea d) => (NP -> NP -> t) -> c -> d -> t
fterms f a b = f (a ^. term) (b ^. term)

--Just to keep the use of (^.) down a bit
-- | Apply a unary function to the term of a named idea, instead of the named
-- idea itself. Ex. @fterm titleize t1@ instead of @titleize $ t1 ^. term@
fterm :: (NamedIdea c) => (NP -> t) -> c -> t
fterm f t1 = f $ t1 ^. term