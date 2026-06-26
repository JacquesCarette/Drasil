-- | Defines concepts used to create documentation.

-- Changes to documentation-related named chunks and common ideas should be reflected in the
-- 'Creating Your Project in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module Data.Drasil.Concepts.Documentation (
    module Drasil.Metadata.Documentation
  , module Data.Drasil.Concepts.Documentation
  ) where

import Drasil.Database (mkUid)
import Language.Drasil hiding (organization, year, label, variable, sec)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.Metadata.Domains (softEng)
import Drasil.Metadata.Documentation
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
  likelyChg, learnObj, mg, mis, notebook, physSyst, refBy, refName, requirement,
  srs, thModel, typUnc, unlikelyChg]

-- * Common Ideas

dataConst :: CI

----------------------------------------------------------------------------------------------------------------------
-- | CI       |          |    uid              |        term                            | abbreviation | ConceptDomain
----------------------------------------------------------------------------------------------------------------------
dataConst   = commonIdea (mkUid "dataConst")   (cn' "data constraint")                       "DC"      [softEng]

---------------------------------------------------------------------

-- concepts relating to the templates and their contents
-- * Named Chunks
-- ** Basic Chunks

analysis, aspect, body, class_, company, concept, condition, connection, constant,
  consumer, coordinate, effect, emphasis, failure, figure, first, game,
  instance_, issue, label, library, literacy, loss, material_, mainIdea, message, method_,
  name_, notApp, object, open,
  physics, practice, priority, procedure,
  realtime, response,
  result, reviewer, safety, scpOfTheProjS, second_, scenario,
  source, simulation, standard, task, type_, video, year :: IdeaDict

analysis        = idea' (mkUid "analysis")       (cnIS   "analysis"           )
aspect          = idea' (mkUid "aspect")         (cn'    "aspect"             )
body            = idea' (mkUid "body")           (cnIES  "body"               )
class_          = idea' (mkUid "class")          (cn'''  "class"              )
company         = idea' (mkUid "company")        (cnIES  "company"            )
concept         = idea' (mkUid "concept")        (cn'    "concept"            )
condition       = idea' (mkUid "condition")      (cn'    "condition"          )
connection      = idea' (mkUid "connection")     (cn'    "connection"         )
constant        = idea' (mkUid "constant")       (cn'    "constant"           )
consumer        = idea' (mkUid "consumer")       (cn'    "consumer"           )
coordinate      = idea' (mkUid "coordinate")     (cn'    "coordinate"         )
effect          = idea' (mkUid "effect")         (cn'    "effect"             )
emphasis        = idea' (mkUid "emphasis")       (cnIS   "emphasis"           )
failure         = idea' (mkUid "failure")        (cn'    "failure"            )
figure          = idea' (mkUid "figure")         (cn'    "figure"             )
first           = idea' (mkUid "first")          (cn'    "first"              ) --Does it make sense for this to be here?
game            = idea' (mkUid "game")           (cn'    "game"               )
instance_       = idea' (mkUid "instance")       (cn'    "instance"           )
issue           = idea' (mkUid "issue")          (cn'    "issue"              )
label           = idea' (mkUid "label")          (cn'    "label"              )
library         = idea' (mkUid "library")        (cnIES  "library"            )
literacy        = idea' (mkUid "literacy")       (cnIES  "literacy"           )
loss            = idea' (mkUid "loss")           (cn'''  "loss"               )
material_       = idea' (mkUid "material")       (cn'    "material"           )
mainIdea        = idea' (mkUid "mainIdea")       (cn'    "main idea"          )
message         = idea' (mkUid "message")        (cn'    "message"            )
method_         = idea' (mkUid "method")         (cn'    "method"             )
name_           = idea' (mkUid "name")           (cn'    "name"               )
object          = idea' (mkUid "object")         (cn'    "object"             )
open            = idea' (mkUid "open")           (cn'    "open"               )
physics         = idea' (mkUid "physics")        (cn'    "physics"            )
practice        = idea' (mkUid "practice")       (cn'    "practice"           )
priority        = idea' (mkUid "priority")       (cnIES  "priority"           )
procedure       = idea' (mkUid "procedure")      (cn'    "procedure"          )
realtime        = idea' (mkUid "real-time")      (cn'    "real-time"          )
response        = idea' (mkUid "response")       (cn'    "response"           )
result          = idea' (mkUid "result")         (cn'    "result"             )
reviewer        = idea' (mkUid "reviewer")       (cn'    "reviewer"           )
safety          = idea' (mkUid "safety")         (cnIES  "safety"             )
second_         = idea' (mkUid "second")         (cn'    "second"             ) --Does it make sense for this to be here?
scenario        = idea' (mkUid "scenario")       (cn'    "scenario"           )
source          = idea' (mkUid "source")         (cn'    "source"             )
simulation      = idea' (mkUid "simulation")     (cn'    "simulation"         )
standard        = idea' (mkUid "standard")       (cn'    "standard"           )
task            = idea' (mkUid "task")           (cn'    "task"               )
type_           = idea' (mkUid "type")           (cn'    "type"               )
video           = idea' (mkUid "video")          (cn'    "video"              )
year            = idea' (mkUid "year")           (cn'    "year"               )
scpOfTheProjS   = idea' (mkUid "scpOfTheProj")   (cn'    "scope of the project") -- temporary generated for test

notApp          = idea (mkUid "notApp") (nounPhraseSP "not applicable")   "N/A"

methAndAnls, procForAnls :: IdeaDict

methAndAnls         = idea' (mkUid "methAndAnls")        (method_ `and_` analysis)
procForAnls         = idea' (mkUid "procForAnls")        (procedure `for` analysis)

-- ** Compound Chunks

coordinateSystem, physicalSim, useCaseTable, physicalProperty, safetyReq,
  softwareSys, softwareVerif, uncertCol, userInput :: IdeaDict

coordinateSystem             = compoundNC coordinate system
--inDatumConstraint            = compoundNC input_ datumConstraint -- may be used later, but they break stable for now
--outDatumConstraint           = compoundNC output_ datumConstraint
physicalProperty             = compoundNC physical property
physicalSim                  = compoundNC physical simulation
safetyReq                    = compoundNC safety requirement
softwareSys                  = compoundNC software system
softwareVerif                = compoundNC software verification
uncertCol                    = compoundNC uncertainty column
useCaseTable                 = compoundNC useCase table_
userInput                    = compoundNC user input_

-- * Domains

-- | List of SRS-related concepts, including SRS.
srsDomains :: [ConceptChunk]
srsDomains = [srsDom, goalStmtDom, reqDom, funcReqDom, nonFuncReqDom,
  assumpDom, chgProbDom, likeChgDom, unlikeChgDom]
