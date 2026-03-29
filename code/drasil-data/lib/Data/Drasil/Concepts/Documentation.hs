-- | Defines concepts used to create documentation.

-- Changes to documentation-related named chunks and common ideas should be reflected in the
-- 'Creating Your Project in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module Data.Drasil.Concepts.Documentation (
    module Drasil.Metadata.Documentation
  , module Data.Drasil.Concepts.Documentation
  ) where

import Control.Lens ((^.))

import Language.Drasil hiding (organization, year, label, variable, sec)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.Metadata.Domains (softEng, documentc)
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
  likelyChg, learnObj, mg, mis, notebook, physSyst, refBy, requirement, srs,
  thModel, typUnc, unlikelyChg]

dataConst, refName :: CI

-- * Common Ideas

------------------------------------------------------------------------------------------------------------------------------
-- | CI       |                  |    uid      |         term                                   | abbreviation | ConceptDomain
------------------------------------------------------------------------------------------------------------------------------
dataConst   = commonIdeaWithDict "dataConst"   (cn' "data constraint")                               "DC"      [softEng]
refName     = commonIdeaWithDict "refName"     (cn' "reference name")                                "Refname" [documentc]

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

analysis        = nc "analysis"       (cnIS   "analysis"           )
aspect          = nc "aspect"         (cn'    "aspect"             )
body            = nc "body"           (cnIES  "body"               )
class_          = nc "class"          (cn'''  "class"              )
company         = nc "company"        (cnIES  "company"            )
concept         = nc "concept"        (cn'    "concept"            )
condition       = nc "condition"      (cn'    "condition"          )
connection      = nc "connection"     (cn'    "connection"         )
constant        = nc "constant"       (cn'    "constant"           )
consumer        = nc "consumer"       (cn'    "consumer"           )
coordinate      = nc "coordinate"     (cn'    "coordinate"         )
effect          = nc "effect"         (cn'    "effect"             )
emphasis        = nc "emphasis"       (cnIS   "emphasis"           )
failure         = nc "failure"        (cn'    "failure"            )
figure          = nc "figure"         (cn'    "figure"             )
first           = nc "first"          (cn'    "first"              ) --Does it make sense for this to be here?
game            = nc "game"           (cn'    "game"               )
instance_       = nc "instance"       (cn'    "instance"           )
issue           = nc "issue"          (cn'    "issue"              )
label           = nc "label"          (cn'    "label"              )
library         = nc "library"        (cnIES  "library"            )
literacy        = nc "literacy"       (cnIES  "literacy"           )
loss            = nc "loss"           (cn'''  "loss"               )
material_       = nc "material"       (cn'    "material"           )
mainIdea        = nc "mainIdea"       (cn'    "main idea"          )
message         = nc "message"        (cn'    "message"            )
method_         = nc "method"         (cn'    "method"             )
name_           = nc "name"           (cn'    "name"               )
object          = nc "object"         (cn'    "object"             )
open            = nc "open"           (cn'    "open"               )
physics         = nc "physics"        (cn'    "physics"            )
practice        = nc "practice"       (cn'    "practice"           )
priority        = nc "priority"       (cnIES  "priority"           )
procedure       = nc "procedure"      (cn'    "procedure"          )
realtime        = nc "real-time"      (cn'    "real-time"          )
response        = nc "response"       (cn'    "response"           )
result          = nc "result"         (cn'    "result"             )
reviewer        = nc "reviewer"       (cn'    "reviewer"           )
safety          = nc "safety"         (cnIES  "safety"             )
second_         = nc "second"         (cn'    "second"             ) --Does it make sense for this to be here?
scenario        = nc "scenario"       (cn'    "scenario"           )
source          = nc "source"         (cn'    "source"             )
simulation      = nc "simulation"     (cn'    "simulation"         )
standard        = nc "standard"       (cn'    "standard"           )
task            = nc "task"           (cn'    "task"               )
type_           = nc "type"           (cn'    "type"               )
video           = nc "video"          (cn'    "video"              )
year            = nc "year"           (cn'    "year"               )
scpOfTheProjS   = nc "scpOfTheProj"   (cn'    "scope of the project") -- temporary generated for test

notApp          = mkIdea "notApp" (nounPhraseSP "not applicable")   (Just "N/A")

methAndAnls, procForAnls :: IdeaDict

methAndAnls         = nc "methAndAnls"        (method_ `and_` analysis)
procForAnls         = nc "procForAnls"        (procedure `for` analysis)

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

refByDom, refNameDom :: ConceptChunk
refByDom      = ccs (mkIdea "refByDom"      (refBy ^. term)                    $ Just "RefBy")    EmptyS [srsDom]
refNameDom    = ccs (mkIdea "refNameDom"    (refName ^. term)                  $ Just "RefName")  EmptyS [srsDom]

-- | List of SRS-related concepts, including SRS.
srsDomains :: [ConceptChunk]
srsDomains = [cw srsDom, goalStmtDom, reqDom, funcReqDom, nonFuncReqDom,
  assumpDom, chgProbDom, likeChgDom, unlikeChgDom, refByDom, refNameDom]
